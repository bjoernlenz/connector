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
    ("migrate":storage:system:database:firstArg:rest) -> do
      specs <- either fail pure (migrationSpecs (firstArg : rest))
      config <- configure storage system database
      migrations <- mapM (uncurry loadMigration) specs
      withLocalConnector config (\c -> runMigrations c migrations)
    [mode, storage, system, database] -> do
      config <- configure storage system database
      case mode of
        "init" -> initialize config
        "demo" -> withLocalConnector config $ \c -> do
          context <- resolveContext c
          _ <- saveNote c context "hello" "Hello from the separate starter"
          findNote c "hello" >>= print
        _ -> fail usage
    _ -> fail usage

migrationSpecs :: [String] -> Either String [(String, FilePath)]
migrationSpecs [] = Right []
migrationSpecs ("--migration":ident:path:rest) | not (null ident) && not (null path) =
  ((ident, path) :) <$> migrationSpecs rest
migrationSpecs _ = Left usage

usage :: String
usage = unlines
  [ "Usage: starter init|demo STORAGE SYSTEM DATABASE"
  , "       starter migrate STORAGE SYSTEM DATABASE --migration ID PATH [--migration ID PATH ...]"
  ]
