module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (unless, forever)
import Datomic.Connector
import Datomic.EDN
import Datomic.Report (decodeTransactionReport)
import Datomic.Runtime (BridgeRuntime(..))
import Data.List (isSuffixOf)
import System.Environment (getArgs, getExecutablePath)
import System.IO (hFlush, stdout)
import System.Timeout (timeout)

check :: Bool -> String -> IO ()
check ok message = unless ok (ioError (userError message))

refused :: IO a -> IO ()
refused action = do
  result <- try (action >> pure ()) :: IO (Either ConnectorError ())
  check (either (const True) (const False) result) "Expected refusal"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-cp":classpath:_) | "mock-ready" `isSuffixOf` classpath -> mock "ready"
    ("-cp":classpath:_) | "mock-mismatch" `isSuffixOf` classpath -> mock "mismatch"
    ("-cp":classpath:_) | "mock-slow" `isSuffixOf` classpath -> mock "slow"
    ["--mock", mode] -> mock mode
    ["--scalars", bridge, storage, system, database] -> scalarTests bridge storage system database
    _ -> tests

mock :: String -> IO ()
mock mode = case mode of
  "ready" -> getLine >> putStrLn "{:ok true :result {:protocol 1 :version \"0.1.0.0\"}}" >> hFlush stdout
  "mismatch" -> getLine >> putStrLn "{:ok true :result {:protocol 999 :version \"0.0.0\"}}" >> hFlush stdout
  "slow" -> do
    _ <- getLine
    threadDelay 500000
    putStrLn "{:ok true :result :abandoned}"
    hFlush stdout
    forever (getLine >> putStrLn "{:ok true :result :next}" >> hFlush stdout)
  "bad-report" -> getLine >> putStrLn "{:ok true :result {:t 1}}" >> hFlush stdout
  "malformed" -> getLine >> putStrLn "garbage" >> hFlush stdout
  "reject" -> do
    _ <- getLine
    putStrLn "{:ok false :error {:message \"rejected\"}}"
    hFlush stdout
    _ <- getLine
    putStrLn "{:ok true :result :recovered}"
    hFlush stdout
  "echo" -> do
    line <- getLine
    request <- either (ioError . userError) pure (parseEDN line)
    putStrLn (renderEDN (Map [(Keyword "ok", Boolean True), (Keyword "result", maybe Nil id (field "query" request))]))
    hFlush stdout
  _ -> pure ()

tests :: IO ()
tests = do
  ednTests
  reportTests
  exe <- getExecutablePath
  let config mode = (defaultLocalConfig "." "test" "test") {bridgeRuntime = JavaClasspath exe mode}
  entered <- withLocalConnector (config "mock-ready") (const (pure True))
  check entered "Compatible bridge should enter caller scope"
  refused (withLocalConnector (config "mock-mismatch") (const (error "Incompatible bridge entered caller scope")))
  refused (withLocalConnector ((config "mock-slow") {startupTimeoutMicros = 100000}) (const (error "Timed-out bridge entered caller scope")))
  withConnector exe ["--mock", "slow"] $ \c -> do
    abandoned <- timeout 100000 (query c (Keyword "first") [])
    check (abandoned == Nothing) "First request should be cancelled"
    refused (query c (Keyword "second") [])
  mapM_ (\mode -> withConnector exe ["--mock", mode] $ \c -> do
    refused (query c Nil [])
    refused (query c Nil [])) ["malformed", "exit"]
  mapM_ (\mode -> withConnector exe ["--mock", mode] $ \c -> do
    result <- try (transact c (WriteContext (EntityId 1) (EntityId 2)) (Vector []))
    check (case result of Left (WriteOutcomeUnknown _) -> True; _ -> False) "Lost write must report unknown outcome"
    result2 <- try (query c Nil [])
    check (case result2 of Left ConnectorClosed -> True; _ -> False) "Failed exchange must close connector") ["malformed", "bad-report"]
  withConnector exe ["--mock", "slow"] $ \c -> do
    cancelled <- timeout 100000 (transact c (WriteContext (EntityId 1) (EntityId 2)) (Vector []))
    check (cancelled == Nothing) "Write cancellation must retain timeout semantics"
  withConnector exe ["--mock", "reject"] $ \c -> do
    rejection <- try (query c Nil [])
    check (case rejection of Left (DatomicRejected e) -> field "message" e == Just (StringEDN "rejected"); _ -> False) "Structured rejection lost"
    result <- query c Nil []
    check (result == Keyword "recovered") "A complete rejection must keep the connector usable"
  closed <- withConnector exe ["--mock", "exit"] pure
  refused (query closed Nil [])
  withConnector exe ["--mock", "echo"] $ \c -> do
    refused (query c (Vector [StringEDN "prefix", Symbol "bad symbol"]) [])
    result <- query c (Keyword "valid") []
    check (result == Keyword "valid") "Encoding failures must not send partial requests"
  putStrLn "PASS: cancelled, malformed and dead bridges close; rejected requests recover"

ednTests :: IO ()
ednTests = do
  mapM_ (\v -> check ((encodeEDN v >>= parseEDN) == Right v) ("Round trip failed: " ++ show v))
    [IntEDN (-9223372036854775808), IntEDN 9223372036854775807,
     BigIntEDN (-99999999999999999999999), FloatEDN (-1.25), FloatEDN 1.0e-100,
     DecimalEDN (-12500) (-4), DecimalEDN 123 100,
     StringEDN "Björn\n\t\x1f600", Tagged "uuid" (StringEDN "550e8400-e29b-41d4-a716-446655440000"),
     Tagged "connector/uri" (StringEDN "https://example.org"), Tagged "connector/bytes" (StringEDN "AAH+/w=="),
     Tagged "connector/float" (StringEDN "0.1"),
     Map [(Keyword "a/b", Set [Boolean False, Nil])]]
  mapM_ (\(text, expected) -> check (parseEDN text == Right expected) ("Wrong number: " ++ text))
    [("-1.25", FloatEDN (-1.25)), ("+1.25", FloatEDN 1.25), ("-42N", BigIntEDN (-42)),
     ("+42", IntEDN 42), ("-1.2500M", DecimalEDN (-12500) (-4)), ("1e+2M", DecimalEDN 1 2),
     ("\"\\uD83D\\uDE00\"", StringEDN "\x1f600")]
  mapM_ (\text -> check (either (const True) (const False) (parseEDN text)) ("Accepted malformed value: " ++ text))
    ["-1/2", "+1e", "-12oops", "9223372036854775808", "1.2N", "\"\\uD800\"", "\"raw\nnewline\""]
  mapM_ (\v -> check (either (const True) (const False) (encodeEDN v)) "Accepted invalid outgoing value")
    [Symbol "nil", Symbol "123", Symbol "-1.25", Keyword "bad name", IntEDN 9223372036854775808]
  check (case parseEDN "##NaN" of Right (FloatEDN n) -> isNaN n; _ -> False) "NaN parse failed"
  mapM_ (\n -> check ((encodeEDN (FloatEDN n) >>= parseEDN) == Right (FloatEDN n)) "Infinity round trip failed") [1/0, -1/0]
  putStrLn "PASS: EDN numeric, Unicode and encoding validation tests"

-- Run against a separately provisioned database; no application schema is assumed.
scalarTests :: FilePath -> FilePath -> String -> String -> IO ()
scalarTests bridge storage system database = withConnector bridge ["serve", storage, system, database] $ \c -> do
  let literal = either error id . parseEDN
  a <- pull c (literal "[:db/id]") (Keyword "identity/bjoern")
  v <- pull c (literal "[:db/id]") (Keyword "identity/conversation-app")
  let entity m = case field "db/id" m of Just (IntEDN i) -> EntityId i; _ -> error "Missing fixture identity"
      context = WriteContext (entity a) (entity v)
      values = [("long", IntEDN (-42)), ("bigint", BigIntEDN (-99999999999999999999999)),
                ("double", FloatEDN (-1.25)), ("float", Tagged "connector/float" (StringEDN "0.1")),
                ("bigdec", DecimalEDN (-12500) (-4)), ("boolean", Boolean True),
                ("string", StringEDN "Björn\n\x1f600"), ("keyword", Keyword "sample/value"),
                ("uuid", Tagged "uuid" (StringEDN "550e8400-e29b-41d4-a716-446655440000")),
                ("instant", Tagged "inst" (StringEDN "2026-01-01T00:00:00.000-00:00")),
                ("uri", Tagged "connector/uri" (StringEDN "https://example.org/a?b=c")),
                ("symbol", Symbol "sample/value"),
                ("ref", case field "db/id" a of Just i -> i; _ -> Nil),
                ("tuple", Vector [IntEDN 1, IntEDN 2])]
      attr kind = Keyword ("scalar/" ++ kind)
      schema (kind, _) = Map ([(Keyword "db/ident", attr kind),
                              (Keyword "db/valueType", Keyword ("db.type/" ++ kind)),
                              (Keyword "db/cardinality", Keyword "db.cardinality/one")]
                              ++ [(Keyword "db/tupleType", Keyword "db.type/long") | kind == "tuple"])
  _ <- transact c context (Vector (map schema values))
  report <- transactReport c context (Vector [Map ((Keyword "db/id", StringEDN "sample") : [(attr kind, value) | (kind,value) <- values])])
  let eid = case lookup (StringTempId "sample") (transactionTempids report) of Just (EntityId n) -> IntEDN n; _ -> Nil
  check (transactionBasis report > Basis 0 && not (null (transactionDatoms report))) "Missing typed report basis/datoms"
  let EntityId txid = transactionEntity report
  metadata <- pull c (literal "[{:write/actor [:db/id]} {:write/via [:db/id]} :db/txInstant]") (IntEDN txid)
  check ((field "write/actor" metadata >>= field "db/id") == field "db/id" a) "Report metadata actor mismatch"
  result <- pull c (literal "[*]") eid
  mapM_ (\(kind, expected) -> do
    let actual = field ("scalar/" ++ kind) result
        normalized = if kind == "ref" then actual >>= field "db/id" else actual
    check (normalized == Just expected) ("Scalar mismatch: " ++ kind ++ " in " ++ show result)) values
  mapM_ (\(kind, value) -> do
    rows <- query c (literal "[:find ?e :in $ ?attribute ?value :where [?e ?attribute ?value]]") [attr kind, value]
    check (case rows of Vector found -> Vector [eid] `elem` found; _ -> False)
      ("Scalar cannot be reused as an equality query input: " ++ kind ++ " in " ++ show rows)) values
  rejection <- try (transactReport c context (literal "[[:db/add 1 :missing/attribute 42]]"))
  check (case rejection of Left (DatomicRejected e) -> field "data" e /= Nothing; _ -> False) "Datomic rejection must retain exception data"
  _ <- query c (literal "[:find ?e :where [?e :db/ident :write/actor]]") []
  escaped <- withSnapshot c $ \snapshot -> do
    before <- pullSnapshot snapshot (literal "[:scalar/string]") eid
    changed <- transactReport c context (Vector [Vector [Keyword "db/add", eid, attr "string", StringEDN "changed"]])
    check (transactionBasis changed > snapshotBasis snapshot) "Snapshot basis must precede write"
    latest <- pull c (literal "[:scalar/string]") eid
    pinned <- pullSnapshot snapshot (literal "[:scalar/string]") eid
    rows <- querySnapshot snapshot (literal "[:find ?s :in $ ?e :where [?e :scalar/string ?s]]") [eid]
    check (before == pinned && latest /= pinned) "Snapshot changed after write"
    check (rows == Vector [Vector [maybe Nil id (field "scalar/string" before)]]) "Snapshot query differs from pull"
    pure snapshot
  refused (pullSnapshot escaped (literal "[*]") eid)
  putStrLn "PASS: snapshot query/pull remain pinned while ordinary reads advance; released handle refused"
  putStrLn "PASS: Datomic scalar types round trip through transactions, pull and equality queries"

reportTests :: IO ()
reportTests = do
  let decode = decodeTransactionReport . either error id . parseEDN
      good = "{:t 1 :tx 100 :tempids {\"x\" 200} :tx-data [[0 10 #connector/float \"0.1\" 100 true]]}"
  check (case decode good of Right r -> transactionBasis r == Basis 1 && map datomValue (transactionDatoms r) == [Tagged "connector/float" (StringEDN "0.1")]; _ -> False) "Report scalar lost"
  mapM_ (\text -> check (case decode text of Left _ -> True; _ -> False) "Accepted malformed report")
    ["nil", "{:t 1}", "{:t -1 :tx 100 :tempids {} :tx-data []}",
     "{:t 1 :tx 100 :tempids {} :tx-data [[2 3 4 101 true]]}",
     "{:t 1 :tx 100 :tempids {} :tx-data [[2 3 4 100 1]]}"]
