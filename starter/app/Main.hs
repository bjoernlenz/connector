module Main where

import Datomic.Connector
import Starter.Config
import Starter.Migrations
import Starter.Persistence
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("migrate":storage:system:database:files) | not (null files) -> do
      config <- configure storage system database
      migrations <- mapM (\file -> loadMigration file file) files
      withLocalConnector config (\c -> runMigrations c migrations)
    [mode, storage, system, database] -> do
      config <- configure storage system database
      case mode of
        "init" -> initialize config
        "demo" -> withLocalConnector config $ \c -> do
          context <- resolveContext c
          _ <- saveNote c context "hello" "Hello from the separate starter"
          findNote c "hello" >>= print
        _ -> fail "Expected init or demo"
    _ -> fail "Usage: starter init|demo|migrate STORAGE SYSTEM DATABASE [MIGRATION.edn ...]"
