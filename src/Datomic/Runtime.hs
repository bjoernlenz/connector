-- Runtime discovery shared by the library and the operational executable.
module Datomic.Runtime (BridgeRuntime(..), bridgeRuntimeFromEnvironment, bridgeProcess) where

import Datomic.EDN
import Data.Version (showVersion)
import Paths_connector (getDataFileName, version)
import System.Directory (getXdgDirectory, XdgDirectory(XdgCache), createDirectoryIfMissing, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>), searchPathSeparator, isPathSeparator)
import System.Process (CreateProcess(..), proc)

data BridgeRuntime
  = ClojureCLI FilePath
  -- Classpath contains dependency jars; the matching bundled bridge is prepended.
  | JavaClasspath FilePath String
  deriving (Eq, Show)

bridgeRuntimeFromEnvironment :: IO BridgeRuntime
bridgeRuntimeFromEnvironment = do
  classpath <- lookupEnv "DATOMIC_CONNECTOR_CLASSPATH"
  javaOverride <- lookupEnv "DATOMIC_CONNECTOR_JAVA"
  javaHome <- lookupEnv "JAVA_HOME"
  clojure <- lookupEnv "DATOMIC_CONNECTOR_CLOJURE"
  let java = maybe (maybe "java" (</> "bin/java") javaHome) id javaOverride
  pure $ maybe (ClojureCLI (maybe "clojure" id clojure)) (JavaClasspath java) classpath

bridgeProcess :: BridgeRuntime -> [String] -> IO CreateProcess
bridgeProcess runtime args = do
  source <- getDataFileName "bridge/src"
  let mainArgs = ["-m", "connector.main"] ++ args
  case runtime of
    JavaClasspath java classpath -> pure (proc java (["-cp", source ++ [searchPathSeparator] ++ classpath, "clojure.main"] ++ mainArgs))
    ClojureCLI clojure -> do
      -- Run in our writable cache, never in the consuming project's deps.edn or
      -- the installed package's (potentially read-only) resource directory.
      cache <- getXdgDirectory XdgCache ("haskell-datomic-connector" </> showVersion version)
      createDirectoryIfMissing True cache
      depsFile <- getDataFileName "deps.edn"
      config <- readFile depsFile >>= either (ioError . userError) pure . parseEDN
      deps <- maybe (ioError (userError "Bundled deps.edn lacks :deps")) pure (field "deps" config)
      let isolated = Map ([(Keyword "paths", Vector [StringEDN source]), (Keyword "deps", deps)]
                         ++ [(Keyword "mvn/repos", repos) | Just repos <- [field "mvn/repos" config]])
      command <- if any isPathSeparator clojure then makeAbsolute clojure else pure clojure
      pure (proc command (["-Srepro", "-Sdeps", renderEDN isolated, "-M"] ++ mainArgs)) {cwd = Just cache}
