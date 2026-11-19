module Starter.Config (configure, initialize) where

import Datomic.Connector
import Datomic.Runtime
import Paths_connector_starter (getDataFileName)
import System.Directory (makeAbsolute)
import System.Process (readCreateProcess)

configure :: FilePath -> String -> String -> IO LocalConfig
configure storage system database = do
  runtime <- bridgeRuntimeFromEnvironment
  absolute <- makeAbsolute storage
  pure (defaultLocalConfig absolute system database) {bridgeRuntime = runtime}

-- Operational path only. Existing databases are refused by the connector.
initialize :: LocalConfig -> IO ()
initialize config = do
  files <- mapM getDataFileName ["db/schema.edn", "db/identities.edn"]
  command <- bridgeProcess (bridgeRuntime config)
    (["provision", storageDirectory config, systemName config, databaseName config] ++ files)
  readCreateProcess command "" >>= putStr
