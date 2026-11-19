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
    (mode:storage:system:database:files)
      | mode == "provision" || (mode == "serve" && null files) -> do
          absoluteStorage <- makeAbsolute storage
          absoluteFiles <- mapM makeAbsolute files
          runtime <- bridgeRuntimeFromEnvironment
          launch <- bridgeProcess runtime ([mode, absoluteStorage, system, database] ++ absoluteFiles)
          (_, _, _, process) <- createProcess launch
          waitForProcess process >>= exitWith
    _ -> ioError (userError "Usage: datomic-bridge provision STORAGE SYSTEM DATABASE [INITIAL-TX.edn ...] | datomic-bridge serve STORAGE SYSTEM DATABASE")
