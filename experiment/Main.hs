module Main where

import Control.Exception (try, IOException)
import Control.Monad (unless)
import Datomic.Connector
import Datomic.EDN
import Datomic.Runtime (bridgeRuntimeFromEnvironment)
import System.Environment (getArgs)

edn :: String -> EDN
edn = either error id . parseEDN

check :: Bool -> String -> IO ()
check ok message = unless ok (ioError (userError message))

required :: String -> EDN -> IO EDN
required key v = maybe (ioError (userError ("Missing " ++ key))) pure (field key v)

identity :: Connector -> String -> IO EntityId
identity c ident = do
  p <- pull c (edn "[:db/id]") (Keyword ident)
  i <- required "db/id" p
  case i of
    IntEDN n -> pure (EntityId n)
    _ -> ioError (userError "Identity is not a numeric entity ID")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, storage, system, db, key] -> do
      runtime <- bridgeRuntimeFromEnvironment
      withLocalConnector ((defaultLocalConfig storage system db) {bridgeRuntime = runtime}) $ \c -> do
        a <- identity c "identity/bjoern"
        v <- identity c "identity/conversation-app"
        case mode of
          "write" -> do
            r <- transact c (WriteContext a v) (Vector [Map
              [(Keyword "db/id", StringEDN "sample"),
               (Keyword "test/key", StringEDN key),
               (Keyword "test/value", StringEDN "Hello from Haskell: Björn")]])
            putStrLn ("Transaction report: " ++ renderEDN r)
            invalid <- try (transact c (WriteContext (EntityId 0) v) (Vector [])) :: IO (Either IOException EDN)
            check (either (const True) (const False) invalid) "Invalid context was accepted"
            unknown <- try (transact c (WriteContext (EntityId 999999999999) v) (Vector [])) :: IO (Either IOException EDN)
            check (either (const True) (const False) unknown) "Unknown actor was accepted"
            override <- try (transact c (WriteContext a v)
              (edn "[{:db/id \"datomic.tx\" :write/actor 1}]")) :: IO (Either IOException EDN)
            check (either (const True) (const False) override) "Metadata override was accepted"
            putStrLn "PASS: invalid context, nonexistent actor and attribution override refused"
          "read" -> pure ()
          _ -> ioError (userError "Mode must be write or read")
        rows <- query c (edn "[:find ?e ?tx :in $ ?key :where [?e :test/key ?key] [?e :test/value ?value ?tx]]") [StringEDN key]
        putStrLn ("Query: " ++ renderEDN rows)
        (eid,txid) <- case rows of
          Vector [Vector [e,t]] -> pure (e,t)
          _ -> ioError (userError "Expected exactly one entity and transaction")
        entity <- pull c (edn "[*]") eid
        check (field "test/value" entity == Just (StringEDN "Hello from Haskell: Björn")) "Entity read mismatch"
        tx <- pull c (edn "[:db/id :db/txInstant {:write/actor [:db/id :identity/name]} {:write/via [:db/id :identity/name]}]") txid
        let EntityId actorId = a
            EntityId viaId = v
        actorMap <- required "write/actor" tx
        viaMap <- required "write/via" tx
        check (field "db/id" actorMap == Just (IntEDN actorId)) "Wrong transaction actor"
        check (field "db/id" viaMap == Just (IntEDN viaId)) "Wrong transaction via"
        instant <- required "db/txInstant" tx
        check (case instant of Tagged "inst" (StringEDN _) -> True; _ -> False) "Missing transaction instant"
        putStrLn ("Entity: " ++ renderEDN entity)
        putStrLn ("Transaction: " ++ renderEDN tx)
        putStrLn "PASS: Haskell query/pull and transaction actor, via, timestamp"
    _ -> ioError (userError "Usage: first-contact write|read STORAGE SYSTEM DATABASE KEY")
