module Main where

import Control.Exception (bracket, try, SomeException, throwIO)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, readMVar)
import Control.Monad (unless)
import Datomic.Connector
import Datomic.EDN
import Starter.Config
import Starter.Migrations
import Starter.Persistence
import System.Directory
import System.IO (openTempFile, hClose)

main :: IO ()
main = bracket temporary removePathForcibly $ \storage -> do
  config <- configure storage "starter-test" "isolated"
  initialize config
  withLocalConnector config $ \c -> do
    context <- resolveContext c
    absent <- findNote c "integration"
    unless (absent == Nothing) (fail "New test database was not empty")
    eid <- saveNote c context "integration" "Attributed note"
    actual <- findNote c "integration"
    unless (actual == Just (Note eid "integration" "Attributed note")) (fail "Persistence mismatch")
    let literal = either error id . parseEDN
    metadata <- query c (literal "[:find ?actor ?via :in $ ?key :where [?e :note/key ?key ?tx] [?tx :write/actor ?actor] [?tx :write/via ?via]]") [StringEDN "integration"]
    let WriteContext (EntityId actorId) (EntityId viaId) = context
    unless (metadata == Vector [Vector [IntEDN actorId, IntEDN viaId]]) (fail "Missing attribution")
    migrationTests storage c context
  putStrLn "PASS: separate starter persistence, project initialization and attributed writes"
  where
    temporary = do
      root <- getTemporaryDirectory
      (path, handle) <- openTempFile root "connector-starter-"
      hClose handle
      removeFile path
      createDirectory path
      pure path

migrationTests :: FilePath -> Connector -> WriteContext -> IO ()
migrationTests storage c context = do
  let literal = either error id . parseEDN
      load ident content = do
        let path = storage ++ "/" ++ ident ++ ".edn"
        writeFile path content
        loadMigration ident path
      count ident = do
        rows <- query c (literal "[:find (count ?e) :in $ ?id :where [?e :migration/id ?id]]") [StringEDN ident]
        pure rows
      revision = pull c (literal "[:migration/revision]") (Keyword "project/migrations")
  initial <- load "001" "[{:db/ident :note/category :db/valueType :db.type/string :db/cardinality :db.cardinality/one}]"
  runMigrations c [initial]
  attribution <- query c (literal "[:find ?actor ?via :in $ ?id :where [?e :migration/id ?id ?tx] [?tx :write/actor ?actor] [?tx :write/via ?via]]") [StringEDN "001"]
  let WriteContext (EntityId a) (EntityId v) = context
  unless (attribution == Vector [Vector [IntEDN a, IntEDN v]]) (fail "Migration attribution missing")
  before <- revision
  beforeBasis <- withSnapshot c (pure . snapshotBasis)
  runMigrations c [initial]
  after <- revision
  afterBasis <- withSnapshot c (pure . snapshotBasis)
  unless (before == after && beforeBasis == afterBasis) (fail "Repeat migration changed revision")
  mismatch <- load "001" "[]"
  refused <- try (runMigrations c [mismatch])
  unless (case refused of Left (LocalValidation _) -> True; _ -> False) (fail "Checksum mismatch accepted")
  broken <- load "broken" "[{:note/key \"must-rollback\" :note/text \"no commit\"} [:db/add 1 :missing/attribute 42]]"
  failure <- try (runMigrations c [broken])
  unless (case failure of Left (DatomicRejected _) -> True; _ -> False) (fail "Expected Datomic rejection")
  missing <- findNote c "must-rollback"
  logged <- count "broken"
  afterFailure <- revision
  unless (missing == Nothing && logged == Vector [] && afterFailure == before) (fail "Failure was not atomic")
  concurrent <- load "concurrent" "[{:note/text \"one migration effect\"}]"
  gate <- newEmptyMVar
  let launch = do
        done <- newEmptyMVar
        _ <- forkIO $ do
          readMVar gate
          result <- try (runMigrations c [concurrent]) :: IO (Either SomeException ())
          putMVar done result
        pure done
  first <- launch
  second <- launch
  putMVar gate ()
  mapM_ (\done -> takeMVar done >>= either throwIO pure) [first, second]
  loggedOnce <- count "concurrent"
  effects <- query c (literal "[:find ?e :where [?e :note/text \"one migration effect\"]]") []
  unless (loggedOnce == Vector [Vector [IntEDN 1]] && case effects of Vector [_] -> True; _ -> False)
    (fail "Concurrent runners applied migration twice")
  -- Force a stale precondition even if scheduling made the runners sequential.
  state <- revision
  old <- case field "migration/revision" state of Just (IntEDN n) -> pure (n - 1); _ -> fail "Missing revision"
  stale <- try (transactReport c context (Vector
    [Vector [Keyword "db/cas", Keyword "project/migrations", Keyword "migration/revision", IntEDN old, IntEDN (old + 1)],
     Map [(Keyword "note/key", StringEDN "stale-effect")],
     Map [(Keyword "migration/id", StringEDN "stale"), (Keyword "migration/checksum", StringEDN "not-applied")]]))
  unless (case stale of Left (DatomicRejected _) -> True; _ -> False) (fail "Stale CAS accepted")
  staleEffect <- findNote c "stale-effect"
  staleLog <- count "stale"
  unless (staleEffect == Nothing && staleLog == Vector []) (fail "Stale CAS committed partial migration")
  putStrLn "PASS: migration apply/repeat/checksum/failure atomicity/concurrent runners/stale CAS"
