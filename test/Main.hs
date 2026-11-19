module Main where

import Control.Concurrent (ThreadId, MVar, forkFinally, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, AsyncException(ThreadKilled), bracket, fromException, try, throwIO)
import System.Exit (exitFailure)
import Control.Monad (unless, forever)
import Datomic.Connector
import Datomic.EDN
import Datomic.Report (decodeTransactionReport)
import Datomic.Runtime (BridgeRuntime(..))
import Data.List (isSuffixOf)
import Data.Time (UTCTime(..), defaultTimeLocale, parseTimeM, addUTCTime, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Environment (getArgs, getExecutablePath)
import System.IO (hClose, hFlush, isEOF, openTempFile, stdout)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeFile, removeDirectoryRecursive)
import System.Timeout (timeout)
import GHC.Conc (ThreadStatus(ThreadBlocked), BlockReason(BlockedOnMVar), threadStatus)

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
    ["--mock-gated", ready, release] -> do
      line <- getLine
      writeFile ready "ready"
      waitUntil (doesFileExist release)
      echoResponse line
      echoLoop
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
  "snapshots" -> snapshotMock 0
  "bad-snapshot" -> getLine >> putStrLn "{:ok true :result {:snapshot \"token\" :t 10 :as-of {:t 9}}}" >> hFlush stdout
  "malformed" -> getLine >> putStrLn "garbage" >> hFlush stdout
  "bad-error" -> getLine >> putStrLn "{:ok false :error nil}" >> hFlush stdout
  "duplicate" -> getLine >> putStrLn "{:ok true :ok false :result nil}" >> hFlush stdout
  "mixed" -> getLine >> putStrLn "{:ok true :result nil :error nil}" >> hFlush stdout
  "exit-failure" -> exitFailure
  "reject" -> do
    _ <- getLine
    putStrLn "{:ok false :error {:class \"test.Rejection\" :message \"rejected\" :data {:cognitect.anomalies/category :cognitect.anomalies/incorrect}}}"
    hFlush stdout
    _ <- getLine
    putStrLn "{:ok true :result :recovered}"
    hFlush stdout
  "echo" -> do
    line <- getLine
    echoResponse line
  "echo-many" -> echoLoop
  _ -> pure ()

-- Count retained handles so callback failure tests verify actual release requests.
snapshotMock :: Integer -> IO ()
snapshotMock retained = do
  done <- isEOF
  unless done $ do
    req <- getLine >>= either fail pure . parseEDN
    let (next, result) = case field "op" req of
          Just (Keyword "snapshot/open") -> (retained + 1,
            Map ([(Keyword "snapshot", StringEDN "token"), (Keyword "t", IntEDN 10)]
              ++ maybe [] (\p -> [(Keyword "as-of", p)]) (field "as-of" req)))
          Just (Keyword "snapshot/close") -> (retained - 1, Nil)
          _ -> (retained, IntEDN retained)
    putStrLn (renderEDN (Map [(Keyword "ok", Boolean True), (Keyword "result", result)]))
    hFlush stdout
    snapshotMock next

echoResponse :: String -> IO ()
echoResponse line = do
  request <- either (ioError . userError) pure (parseEDN line)
  putStrLn (renderEDN (Map [(Keyword "ok", Boolean True), (Keyword "result", maybe Nil id (field "query" request))]))
  hFlush stdout

echoLoop :: IO ()
echoLoop = do
  done <- isEOF
  unless done (getLine >>= echoResponse >> echoLoop)

waitUntil :: IO Bool -> IO ()
waitUntil condition = do
  ready <- condition
  unless ready (threadDelay 1000 >> waitUntil condition)

within :: IO a -> IO a
within action = timeout 10000000 action >>= maybe (fail "Concurrency test timed out") pure

type Worker a = (ThreadId, MVar (Either SomeException a))

withWorker :: IO a -> (Worker a -> IO b) -> IO b
withWorker action = bracket start (killThread . fst)
  where
    start = do
      result <- newEmptyMVar
      tid <- forkFinally action (putMVar result)
      pure (tid, result)

awaitWorker :: Worker a -> IO a
awaitWorker (_, result) = within (takeMVar result) >>= either throwIO pure

awaitCancelled :: Worker a -> IO ()
awaitCancelled (_, result) = do
  outcome <- within (takeMVar result)
  check (case outcome of Left e -> fromException e == Just ThreadKilled; _ -> False)
    "Worker should retain asynchronous cancellation"

-- Observe actual lock contention rather than relying on scheduling delays.
awaitWaiting :: Worker a -> IO ()
awaitWaiting (tid, _) = within $ waitUntil $ do
  status <- threadStatus tid
  pure (status == ThreadBlocked BlockedOnMVar)

withGatedConnector :: FilePath -> (Connector -> FilePath -> FilePath -> IO a) -> IO a
withGatedConnector exe action = bracket temporary removeDirectoryRecursive $ \dir ->
  let ready = dir ++ "/ready"
      release = dir ++ "/release"
  in withConnector exe ["--mock-gated", ready, release] $ \c -> action c ready release
  where
    temporary = do
      dir <- getTemporaryDirectory
      (path, handle) <- openTempFile dir "connector-concurrency"
      hClose handle
      removeFile path
      createDirectory path
      pure path

concurrencyTests :: FilePath -> IO ()
concurrencyTests exe = do
  withConnector exe ["--mock", "echo-many"] $ \c -> do
    gate <- newEmptyMVar
    let start = mapM_ (const (putMVar gate ())) [1..32 :: Int]
    -- Keep every worker in scope until all requests have completed.
    let launch [] = start
        launch (n:ns) = withWorker (takeMVar gate >> query c (IntEDN n) []) $ \worker -> do
          launch ns
          value <- awaitWorker worker
          check (value == IntEDN n) "Concurrent caller received another caller's response"
    within (launch [1..32])
  withGatedConnector exe $ \c ready release ->
    withWorker (query c (Keyword "active") []) $ \active -> do
      within (waitUntil (doesFileExist ready))
      withWorker (query c (Keyword "cancelled-waiter") []) $ \waiting -> do
        awaitWaiting waiting
        killThread (fst waiting)
        awaitCancelled waiting
      writeFile release "release"
      value <- awaitWorker active
      check (value == Keyword "active") "Waiting cancellation affected the active request"
      next <- query c (Keyword "next") []
      check (next == Keyword "next") "Waiting cancellation closed the connector"
  withGatedConnector exe $ \c ready _ ->
    withWorker (transact c (WriteContext (EntityId 1) (EntityId 2)) (Vector [])) $ \active -> do
      within (waitUntil (doesFileExist ready))
      let waitingCall = try (query c (Keyword "waiting") []) :: IO (Either ConnectorError EDN)
          closed result = check (case result of Left ConnectorClosed -> True; _ -> False)
            "Waiting caller should fail with ConnectorClosed"
      withWorker waitingCall $ \first -> withWorker waitingCall $ \second -> do
        awaitWaiting first
        awaitWaiting second
        killThread (fst active)
        awaitCancelled active
        awaitWorker first >>= closed
        awaitWorker second >>= closed
        result <- try (query c (Keyword "later") [])
        closed result
  putStrLn "PASS: shared connector routes concurrent replies and handles waiting/active cancellation"

tests :: IO ()
tests = do
  ednTests
  reportTests
  exe <- getExecutablePath
  concurrencyTests exe
  snapshotTests exe
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
    refused (query c Nil [])) ["malformed", "exit", "bad-error", "duplicate", "mixed"]
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
  original <- try (withConnector exe ["--mock", "exit-failure"] (\_ -> throwIO (LocalValidation "original"))) :: IO (Either ConnectorError ())
  check (case original of Left (LocalValidation "original") -> True; _ -> False) "Cleanup replaced original failure"
  cancelledScope <- timeout 100000 (withConnector exe ["--mock", "slow"] (\c -> query c Nil []))
  check (cancelledScope == Nothing) "Cleanup replaced asynchronous cancellation"
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
  historicalTests c context
  compositeTupleTests c context

compositeTupleTests :: Connector -> WriteContext -> IO ()
compositeTupleTests c context = do
  let literal = either error id . parseEDN
  _ <- transactReport c context (literal "[{:db/ident :partial/first :db/valueType :db.type/string :db/cardinality :db.cardinality/one} {:db/ident :partial/second :db/valueType :db.type/string :db/cardinality :db.cardinality/one} {:db/ident :partial/pair :db/valueType :db.type/tuple :db/tupleAttrs [:partial/first :partial/second] :db/cardinality :db.cardinality/one}]")
  first <- transactReport c context (literal "[{:db/id \"partial\" :partial/first \"one\"}]")
  check (Vector [StringEDN "one", Nil] `elem` map datomValue (transactionDatoms first)) "Partial composite tuple missing from report"
  eid <- case lookup (StringTempId "partial") (transactionTempids first) of
    Just (EntityId n) -> pure (IntEDN n)
    _ -> fail "Missing partial tuple entity"
  second <- transactReport c context (Vector [Vector [Keyword "db/add", eid, Keyword "partial/second", StringEDN "two"]])
  check (Vector [StringEDN "one", StringEDN "two"] `elem` map datomValue (transactionDatoms second)) "Completed composite tuple missing from report"
  result <- pull c (literal "[:partial/pair]") eid
  check (field "partial/pair" result == Just (Vector [StringEDN "one", StringEDN "two"])) "Composite tuple pull mismatch"
  putStrLn "PASS: partial composite tuple report and subsequent write on same connector"

snapshotTests :: FilePath -> IO ()
snapshotTests exe = do
  withConnector exe ["--mock", "snapshots"] $ \c -> do
    mapM_ (\point -> do
      result <- try (withSnapshotAsOf c point (const (pure ())))
      check (case result of Left (LocalValidation _) -> True; _ -> False) "Invalid selector must fail locally")
      [AsOfBasis (Basis (-1)), AsOfBasis (Basis 9223372036854775808),
       AsOfInstant (UTCTime (fromGregorian 2026 1 1) 86400),
       AsOfInstant (posixSecondsToUTCTime 9223372036854776)]
    query c Nil [] >>= \n -> check (n == IntEDN 0) "Invalid selector sent a request"
    withSnapshotAsOf c (AsOfInstant (posixSecondsToUTCTime (-0.0001))) $ \s ->
      check (snapshotAsOf s == Just (AsOfInstant (posixSecondsToUTCTime (-0.001)))) "Negative epoch instant must floor"
    escaped <- newEmptyMVar
    result <- try (withSnapshotAsOf c (AsOfBasis (Basis 3)) $ \s -> do
      putMVar escaped s
      throwIO (LocalValidation "callback")) :: IO (Either ConnectorError ())
    check (case result of Left (LocalValidation "callback") -> True; _ -> False) "Cleanup replaced callback exception"
    takeMVar escaped >>= \s -> refused (pullSnapshot s Nil Nil)
    query c Nil [] >>= \n -> check (n == IntEDN 0) "Callback exception leaked a snapshot token"
    cancelled <- timeout 100000 (withSnapshotAsOf c (AsOfBasis (Basis 3)) (\_ -> threadDelay 1000000))
    check (cancelled == Nothing) "Snapshot callback cancellation changed"
    query c Nil [] >>= \n -> check (n == IntEDN 0) "Cancellation leaked a snapshot token"
  withConnector exe ["--mock", "bad-snapshot"] $ \c -> do
    result <- try (withSnapshotAsOf c (AsOfBasis (Basis 3)) (const (pure ())))
    check (case result of Left (ProtocolFailure _) -> True; _ -> False) "Mismatched historical response accepted"
    resultClosed <- try (query c Nil [])
    check (case resultClosed of Left ConnectorClosed -> True; _ -> False) "Malformed snapshot must close connector"
  putStrLn "PASS: snapshot selector validation, metadata, exception/cancellation release and malformed responses"

historicalTests :: Connector -> WriteContext -> IO ()
historicalTests c context = do
  let literal = either error id . parseEDN
      amount n = DecimalEDN n (-2)
      entity report name = case lookup (StringTempId name) (transactionTempids report) of
        Just (EntityId n) -> IntEDN n
        _ -> error "Missing historical fixture tempid"
      txInstant report = do
        let EntityId tx = transactionEntity report
        metadata <- pull c (literal "[:db/txInstant]") (IntEDN tx)
        case field "db/txInstant" metadata of
          Just (Tagged "inst" (StringEDN value)) ->
            maybe (fail ("Invalid txInstant: " ++ value)) pure
              (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" value :: Maybe UTCTime)
          _ -> fail "Missing historical fixture timestamp"
  first <- transactReport c context (literal "[{:db/id \"historical\" :scalar/bigdec 1.11M :scalar/string \"original\"}]")
  let eid = entity first "historical"
      update n = transactReport c context (Vector [Vector [Keyword "db/add", eid, Keyword "scalar/bigdec", amount n]])
      value s n = do
        pulled <- pullSnapshot s (literal "[:scalar/bigdec]") eid
        rows <- querySnapshot s (literal "[:find ?v :in $ ?e :where [?e :scalar/bigdec ?v]]") [eid]
        check (field "scalar/bigdec" pulled == Just (amount n) && rows == Vector [Vector [amount n]])
          ("Historical query/pull mismatch at " ++ show (snapshotAsOf s))
  t1 <- withSnapshot c $ \s -> do
    check (snapshotAsOf s == Nothing) "Current snapshot has historical cutoff"
    value s 111
    pure (snapshotBasis s)
  instant1 <- txInstant first
  threadDelay 20000
  second <- transactReport c context (Vector
    [Vector [Keyword "db/add", eid, Keyword "scalar/bigdec", amount 115],
     Map [(Keyword "db/id", StringEDN "later"), (Keyword "scalar/bigdec", amount 200)]])
  instant2 <- txInstant second
  threadDelay 20000
  third <- transactReport c context (Vector
    [Vector [Keyword "db/add", eid, Keyword "scalar/bigdec", amount 110],
     Vector [Keyword "db/retract", eid, Keyword "scalar/string", StringEDN "original"]])
  let t2 = transactionBasis second
      t3 = transactionBasis third
      later = entity second "later"
  check (t1 == transactionBasis first && t1 < t2 && t2 < t3 && instant1 < instant2) "Fixture ordering failed"
  mapM_ (\(basis, expected) -> withSnapshotAsOf c (AsOfBasis basis) $ \s -> do
    check (snapshotBasis s == t3 && snapshotAsOf s == Just (AsOfBasis basis)) "Captured basis confused with historical cutoff"
    value s expected) [(t1,111), (t2,115), (t3,110)]
  withSnapshot c (\s -> value s 110)
  withSnapshotAsOf c (AsOfBasis t1) $ \s -> do
    missing <- querySnapshot s (literal "[:find ?v :in $ ?e :where [?e :scalar/bigdec ?v]]") [later]
    absent <- pullSnapshot s (literal "[:scalar/bigdec]") later
    check (missing == Vector [] && field "scalar/bigdec" absent == Nothing) "Later entity present in historical view"
    old <- pullSnapshot s (literal "[:scalar/string]") eid
    check (field "scalar/string" old == Just (StringEDN "original")) "Retracted fact missing from history"
    withSnapshotAsOf c (AsOfBasis t2) (\other -> value other 115)
  withSnapshotAsOf c (AsOfInstant instant1) (\s -> value s 111)
  withSnapshotAsOf c (AsOfInstant (addUTCTime 0.001 instant1)) (\s -> value s 111)
  withSnapshotAsOf c (AsOfInstant instant2) (\s -> value s 115)
  withSnapshotAsOf c (AsOfInstant (addUTCTime 0.0009 instant1)) $ \s -> do
    check (snapshotAsOf s == Just (AsOfInstant instant1)) "UTC instant was not floored to milliseconds"
    value s 111
  mapM_ (\point -> withSnapshotAsOf c point $ \s -> do
    rows <- querySnapshot s (literal "[:find ?e :where [?e :db/ident :write/actor]]") []
    check (rows == Vector []) "Pre-creation view was not empty")
    [AsOfBasis (Basis 0), AsOfInstant (posixSecondsToUTCTime 0)]
  mapM_ (\point -> do
    result <- try (withSnapshotAsOf c point (const (pure ())))
    check (case result of Left (DatomicRejected _) -> True; _ -> False) "Future historical point should be rejected"
    withSnapshot c (\s -> value s 110))
    [AsOfBasis (let Basis n = t3 in Basis (n+1)), AsOfInstant (posixSecondsToUTCTime 4102444800)]
  escaped <- withSnapshotAsOf c (AsOfBasis t1) $ \historical -> withSnapshot c $ \current -> do
    _ <- update 109
    value historical 111
    value current 110
    latest <- pull c (literal "[:scalar/bigdec]") eid
    check (field "scalar/bigdec" latest == Just (amount 109)) "Ordinary reads did not advance"
    pure historical
  refused (querySnapshot escaped Nil [])
  failedHandle <- newEmptyMVar
  failed <- try (withSnapshotAsOf c (AsOfBasis t1) $ \s -> do
    value s 111
    putMVar failedHandle s
    throwIO (LocalValidation "historical callback")) :: IO (Either ConnectorError ())
  check (case failed of Left (LocalValidation "historical callback") -> True; _ -> False) "Historical callback exception replaced"
  takeMVar failedHandle >>= \s -> refused (pullSnapshot s Nil eid)
  withSnapshotAsOf c (AsOfBasis t2) (\s -> value s 115)
  putStrLn "PASS: real historical basis/UTC query and pull, corrections/retractions, stability, metadata and refusal semantics"

reportTests :: IO ()
reportTests = do
  let decode = decodeTransactionReport . either error id . parseEDN
      good = "{:t 1 :tx 100 :tempids {\"x\" 200} :tx-data [[0 10 #connector/float \"0.1\" 100 true]]}"
  check (case decode good of Right r -> transactionBasis r == Basis 1 && map datomValue (transactionDatoms r) == [Tagged "connector/float" (StringEDN "0.1")]; _ -> False) "Report scalar lost"
  check (case decode "{:t 1 :tx 100 :tempids {} :tx-data [[2 3 [\"one\" nil] 100 true]]}" of Right _ -> True; _ -> False) "Partial tuple rejected"
  mapM_ (\text -> check (case decode text of Left _ -> True; _ -> False) "Accepted malformed report")
    ["{:t 1 :tx 100 :tempids {} :tx-data [[2 3 nil 100 true]]}",
     "nil", "{:t 1}", "{:t -1 :tx 100 :tempids {} :tx-data []}",
     "{:t 1 :tx 100 :tempids {} :tx-data [[2 3 4 101 true]]}",
     "{:t 1 :tx 100 :tempids {} :tx-data [[2 3 4 100 1]]}"]
