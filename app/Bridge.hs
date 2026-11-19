module Main where

import Datomic.Runtime
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (createProcess, waitForProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, storage, system, database] | mode `elem` ["serve", "bootstrap"] -> do
      absoluteStorage <- makeAbsolute storage
      runtime <- bridgeRuntimeFromEnvironment
      launch <- bridgeProcess runtime [mode, absoluteStorage, system, database]
      (_, _, _, process) <- createProcess launch
      waitForProcess process >>= exitWith
    _ -> ioError (userError "Usage: datomic-bridge serve|bootstrap STORAGE SYSTEM DATABASE")
