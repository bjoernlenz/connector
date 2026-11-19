(ns connector.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [datomic.client.api :as d]
            [datomic.local :as local]))

(defn read-edn [s]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. s))]
    (let [options {:eof ::eof
                   :readers {'connector/uri #(java.net.URI. %)
                             'connector/float #(Float/parseFloat ^String %)
                             'connector/bytes #(.decode (java.util.Base64/getDecoder) ^String %)}}
          v (edn/read options r)]
      (when (or (= v ::eof) (not= ::eof (edn/read options r)))
        (throw (ex-info "Expected exactly one EDN value" {})))
      v)))

(defn fail [message] (throw (ex-info message {:connector/error :invalid-request})))

(defn report [r]
  {:t (:t (:db-after r))
   :tx (-> r :tx-data first :tx)
   :tempids (:tempids r)
   :tx-data (mapv (fn [datom] (mapv #(nth datom %) (range 5))) (:tx-data r))})

(defn attributed-transact [conn {:keys [context tx-data]}]
  (let [db (d/db conn)]
    (when-not (and (map? context) (= #{:actor :via} (set (keys context))))
      (fail "WriteContext with actor and via is required"))
    (doseq [id (vals context)]
      (when-not (and (integer? id) (pos? id)
                    (seq (d/datoms db {:index :eavt :components [id] :limit 1})))
        (fail "WriteContext references must be existing entity IDs")))
    (when-not (vector? tx-data) (fail "tx-data must be a vector"))
    ;; The caller cannot override bridge-owned transaction metadata, even in nested maps.
    (walk/postwalk (fn [v]
                     (when (contains? #{:write/actor :write/via :db/txInstant "datomic.tx"} v)
                       (fail "Transaction attribution and timestamp are bridge-owned"))
                     v) tx-data)
    ;; Only native data operations are needed here; no user transaction functions.
    (doseq [entry tx-data]
      (when-not (or (map? entry)
                    (and (vector? entry) (contains? #{:db/add :db/retract :db/cas :db/retractEntity} (first entry))))
        (fail "Expected entity maps or native add/retract/cas/retractEntity operations")))
    (report (d/transact conn {:tx-data (conj tx-data {:db/id "datomic.tx"
                                                    :write/actor (:actor context)
                                                    :write/via (:via context)})}))))

(defn read-db [conn snapshots req]
  (if (contains? req :snapshot)
    (or (get @snapshots (:snapshot req)) (fail "Unknown or released snapshot"))
    (d/db conn)))

(defn open-snapshot [conn snapshots req]
  (let [db (d/db conn)
        historical? (contains? req :as-of)
        selector (:as-of req)
        view (if historical?
               (do
                 (when-not (and (map? selector)
                                (contains? #{#{:t} #{:instant}} (set (keys selector))))
                   (fail "as-of requires exactly t or instant (UTC epoch milliseconds)"))
                 (let [[kind point] (first selector)]
                   (when-not (and (integer? point) (<= Long/MIN_VALUE point Long/MAX_VALUE))
                     (fail "as-of point must be a signed 64-bit integer"))
                   (when (and (= kind :t) (or (neg? point) (> point (:t db))))
                     (fail "as-of basis must be between zero and the captured database basis"))
                   (d/as-of db (if (= kind :instant) (java.util.Date. (long point)) (long point)))))
               db)
        token (str (java.util.UUID/randomUUID))]
    (swap! snapshots assoc token view)
    (cond-> {:snapshot token :t (:t db)}
      historical? (assoc :as-of selector))))

(defn dispatch [conn snapshots package-version req]
  (case (:op req)
    :hello {:protocol 1 :version package-version}
    :snapshot/open (open-snapshot conn snapshots req)
    :snapshot/close (do (when-not (contains? @snapshots (:snapshot req))
                          (fail "Unknown or released snapshot"))
                        (swap! snapshots dissoc (:snapshot req))
                        nil)
    :transact (attributed-transact conn req)
    :query (d/q {:query (:query req) :args (into [(read-db conn snapshots req)] (:args req))})
    :pull (d/pull (read-db conn snapshots req) {:selector (:selector req) :eid (:eid req)})
    (fail "Unknown operation (provisioning is a separate process mode)")))

(defn error-response [e]
  {:ok false :error {:class (.getName (class e)) :message (.getMessage e)
                    :data (ex-data e)}})

;; Clojure's printer leaves some ISO controls literal inside strings. Escape
;; them after printing so every response remains strict, single-line EDN.
(def control-escapes
  (into {} (for [n (concat (range 32) (range 127 160))]
             [(char n) (format "\\u%04x" n)])))

(defn emit [v]
  (binding [*print-length* nil *print-level* nil *print-namespace-maps* false]
    ;; URI and byte[] do not have readable EDN representations in Clojure.
    (println (str/escape (pr-str (walk/postwalk
           (fn [x]
             (cond
               (instance? java.math.BigInteger x) (bigint x)
               ;; EDN's ordinary floating literal reads as Double, which cannot
               ;; match e.g. a stored Float 0.1 in an equality query.
               (instance? Float x) (tagged-literal 'connector/float (Float/toString x))
               (instance? java.net.URI x) (tagged-literal 'connector/uri (str x))
               (bytes? x) (tagged-literal 'connector/bytes (.encodeToString (java.util.Base64/getEncoder) x))
               :else x)) v)) control-escapes))
    (flush)))

(defn provision [client db-name storage-dir paths]
  ;; Parse every initial file before creating anything. Files are explicit inputs
  ;; from the project; the connector owns only the attribution schema.
  (let [transactions (mapv (fn [path]
                             (let [tx (read-edn (slurp path))]
                               (when-not (vector? tx) (fail "Provisioning files must contain transaction vectors"))
                               tx))
                           (cons (io/resource "connector/attribution.edn") paths))]
    ;; create-database always returns true, even for an existing database. Guard
    ;; the explicit existence check and all initialization against other invocations.
    (.mkdirs (io/file storage-dir))
    (with-open [channel (java.nio.channels.FileChannel/open
                          (.toPath (io/file storage-dir ".connector-provision.lock"))
                          (into-array java.nio.file.OpenOption
                            [java.nio.file.StandardOpenOption/CREATE java.nio.file.StandardOpenOption/WRITE]))]
      (let [held (.tryLock channel)]
        (when-not held (fail "Another connector provisioning command is already in progress"))
        (with-open [lock held]
          (when (some #{db-name} (d/list-databases client {:limit -1}))
            (fail "Database already exists; apply subsequent changes with attributed transactions"))
          (d/create-database client {:db-name db-name})
          (let [conn (d/connect client {:db-name db-name})]
            (doseq [tx transactions]
              (d/transact conn {:tx-data tx}))
            (emit {:ok true :result {:db-name db-name :t (:t (d/db conn))
                                    :initial-transactions (count transactions)}})))))))

(defn -main [metadata mode storage-dir system db-name & paths]
  (when-not (and (#{"provision" "serve"} mode) storage-dir system db-name
                 (or (= mode "provision") (empty? paths)))
    (throw (ex-info "Usage: provision STORAGE-DIR SYSTEM DB-NAME [INITIAL-TX.edn ...] | serve STORAGE-DIR SYSTEM DB-NAME" {})))
  (let [package-version (or (second (re-find #"(?m)^version:[ \t]*([0-9]+(?:\.[0-9]+)*)" (slurp metadata)))
                            (fail "Missing version in connector.cabal"))
        client (d/client {:server-type :datomic-local :system system
                         :storage-dir (.getCanonicalPath (io/file storage-dir))})]
    (try
      (case mode
        "provision" (provision client db-name storage-dir paths)
        "serve" (let [conn (d/connect client {:db-name db-name})
                      snapshots (atom {})]
                  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
                    (emit (try {:ok true :result (dispatch conn snapshots package-version (read-edn line))}
                               (catch Exception e (error-response e)))))))
      (finally
        (local/release-db {:system system :db-name db-name})
        (shutdown-agents)))))
