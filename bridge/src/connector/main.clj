(ns connector.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [datomic.client.api :as d]
            [datomic.local :as local]))

(defn read-edn [s]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. s))]
    (let [v (edn/read {:eof ::eof} r)]
      (when (or (= v ::eof) (not= ::eof (edn/read {:eof ::eof} r)))
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

(defn dispatch [conn req]
  (case (:op req)
    :transact (attributed-transact conn req)
    :query (d/q {:query (:query req) :args (into [(d/db conn)] (:args req))})
    :pull (d/pull (d/db conn) {:selector (:selector req) :eid (:eid req)})
    (fail "Unknown operation (bootstrap is a separate process mode)")))

(defn error-response [e]
  {:ok false :error {:class (.getName (class e)) :message (.getMessage e)
                    :data (ex-data e)}})

(defn emit [v]
  (binding [*print-length* nil *print-level* nil *print-namespace-maps* false]
    (prn v) (flush)))

(defn bootstrap [client db-name]
  (d/create-database client {:db-name db-name})
  (let [conn (d/connect client {:db-name db-name})]
    ;; Fixed first-contact bootstrap, deliberately absent from the normal protocol.
    (doseq [path ["experiment/schema.edn" "experiment/identities.edn"]]
      (d/transact conn {:tx-data (read-edn (slurp path))}))
    (emit {:ok true :result {:actor (d/pull (d/db conn) '[*] :identity/bjoern)
                            :via (d/pull (d/db conn) '[*] :identity/conversation-app)}})))

(defn -main [mode storage-dir system db-name]
  (when-not (and (#{"bootstrap" "serve"} mode) storage-dir system db-name)
    (throw (ex-info "Usage: bootstrap|serve STORAGE-DIR SYSTEM DB-NAME" {})))
  (let [client (d/client {:server-type :datomic-local :system system
                         :storage-dir (.getCanonicalPath (io/file storage-dir))})]
    (try
      (case mode
        "bootstrap" (bootstrap client db-name)
        "serve" (let [conn (d/connect client {:db-name db-name})]
                  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
                    (emit (try {:ok true :result (dispatch conn (read-edn line))}
                               (catch Exception e (error-response e)))))))
      (finally
        (local/release-db {:system system :db-name db-name})
        (shutdown-agents)))))
