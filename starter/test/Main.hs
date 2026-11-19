module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import Datomic.Connector
import Datomic.EDN
import Starter.Config
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
  putStrLn "PASS: separate starter persistence, project initialization and attributed writes"
  where
    temporary = do
      root <- getTemporaryDirectory
      (path, handle) <- openTempFile root "connector-starter-"
      hClose handle
      removeFile path
      createDirectory path
      pure path
