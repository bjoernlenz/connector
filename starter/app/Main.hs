module Main where

import Datomic.Connector
import Starter.Config
import Starter.Persistence
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, storage, system, database] -> do
      config <- configure storage system database
      case mode of
        "init" -> initialize config
        "demo" -> withLocalConnector config $ \c -> do
          context <- resolveContext c
          _ <- saveNote c context "hello" "Hello from the separate starter"
          findNote c "hello" >>= print
        _ -> fail "Expected init or demo"
    _ -> fail "Usage: starter init|demo STORAGE SYSTEM DATABASE"
