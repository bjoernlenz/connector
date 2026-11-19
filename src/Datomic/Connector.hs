module Datomic.Connector
  (Connector, EntityId(..), WriteContext(..), withConnector,
   LocalConfig(..), defaultLocalConfig, withLocalConnector, transact, query, pull) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, IOException, mask, try, throwIO, catch, toException)
import Control.Monad (void)
import Datomic.EDN
import Datomic.Runtime
import Data.Version (showVersion)
import Paths_connector (version)
import System.Directory (makeAbsolute)
import System.Exit (ExitCode(..))
import System.IO
import System.Process hiding (system)
import System.Timeout (timeout)

newtype EntityId = EntityId Integer deriving (Eq, Show)
data WriteContext = WriteContext { actor :: EntityId, via :: EntityId } deriving (Eq, Show)
-- A failed exchange permanently closes the protocol: its reply may still arrive.
data Connector = Connector Handle Handle ProcessHandle (MVar Bool)

data LocalConfig = LocalConfig
  { storageDirectory :: FilePath
  , systemName :: String
  , databaseName :: String
  , bridgeRuntime :: BridgeRuntime
  , startupTimeoutMicros :: Int
  } deriving (Eq, Show)

defaultLocalConfig :: FilePath -> String -> String -> LocalConfig
defaultLocalConfig storage system database = LocalConfig storage system database (ClojureCLI "clojure") 60000000

-- Resource paths come from Cabal, not the caller's working directory.
withLocalConnector :: LocalConfig -> (Connector -> IO a) -> IO a
withLocalConnector config action = do
  storage <- makeAbsolute (storageDirectory config)
  process <- bridgeProcess (bridgeRuntime config) ["serve", storage, systemName config, databaseName config]
  withConnectorProcess process $ \connector -> do
    ready <- timeout (startupTimeoutMicros config) (request connector [("op", Keyword "hello")])
    case ready of
      Just info | field "protocol" info == Just (IntEDN 1)
               && field "version" info == Just (StringEDN (showVersion version)) -> action connector
      Nothing -> throwIO (userError "Bridge startup timed out")
      _ -> throwIO (userError "Bridge protocol/package version mismatch")

-- The command must launch the bridge in serve mode; stderr stays visible.
withConnector :: FilePath -> [String] -> (Connector -> IO a) -> IO a
withConnector command args = withConnectorProcess (proc command args)

withConnectorProcess :: CreateProcess -> (Connector -> IO a) -> IO a
withConnectorProcess launch action = mask $ \restore -> do
  connector <- start
  result <- tryAny (restore (action connector))
  cleanup <- tryAny (stop connector)
  -- Cleanup must not replace the original exception (including cancellation).
  either throwIO (\value -> either throwIO (const (pure value)) cleanup) result
  where
    start = do
      (Just input, Just output, _, process) <- createProcess launch
        {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
      lock <- newMVar True
      let connector = Connector input output process lock
      setup <- tryAny (mapM_ (\h -> hSetEncoding h utf8 >> hSetBuffering h LineBuffering) [input,output])
      either (\e -> stop connector >> throwIO e) (const (pure connector)) setup
    stop (Connector input output process lock) = do
      healthy <- modifyMVar lock (\open -> pure (False, open))
      ignoreIO (hClose input)
      code <- timeout 2000000 (waitForProcess process)
      case code of
        Nothing -> do
          ignoreIO (terminateProcess process)
          void (timeout 2000000 (waitForProcess process))
        _ -> pure ()
      ignoreIO (hClose output)
      case code of
        Just ExitSuccess -> pure ()
        Just exit | healthy -> throwIO (userError ("Bridge exited: " ++ show exit))
        Nothing | healthy -> throwIO (userError "Bridge did not shut down within two seconds")
        _ -> pure ()

transact :: Connector -> WriteContext -> EDN -> IO EDN
transact c (WriteContext (EntityId a) (EntityId v)) tx
  | a <= 0 || v <= 0 = throwIO (userError "WriteContext requires positive entity IDs")
  | otherwise = request c
      [("op", Keyword "transact"), ("context", Map [(Keyword "actor", IntEDN a), (Keyword "via", IntEDN v)]),
       ("tx-data", tx)]

-- The bridge supplies the current database as the first query input ($).
query :: Connector -> EDN -> [EDN] -> IO EDN
query c q inputs = request c [("op", Keyword "query"), ("query", q), ("args", Vector inputs)]

pull :: Connector -> EDN -> EDN -> IO EDN
pull c selector eid = request c [("op", Keyword "pull"), ("selector", selector), ("eid", eid)]

-- Private: callers cannot send arbitrary protocol operations or omit context.
request :: Connector -> [(String, EDN)] -> IO EDN
request (Connector input output process lock) entries = do
  -- Validate the entire value before writing even one byte to the stream.
  encoded <- either (throwIO . userError) pure (encodeEDN (Map [(Keyword k,v) | (k,v) <- entries]))
  exchange encoded
 where
  exchange encoded = mask $ \restore -> do
    outcome <- modifyMVar lock $ \healthy ->
      if not healthy then pure (False, Left (toException (userError "Connector is closed after an interrupted or failed exchange; transaction outcome may be unknown")))
      else do
        result <- tryAny $ restore $ do
          hPutStrLn input encoded
          line <- hGetLine output
          response <- either (throwIO . userError) pure (parseEDN line)
          case field "ok" response of
            Just (Boolean True) -> maybe (throwIO (userError "Bridge response lacks result")) (pure . Right) (field "result" response)
            Just (Boolean False) | Just err <- field "error" response -> pure (Left (userError ("Datomic bridge: " ++ renderEDN err)))
            _ -> throwIO (userError "Malformed bridge response")
        case result of
          Right response -> pure (True, Right response)
          Left exception -> do
            ignoreIO (terminateProcess process)
            -- Commit the closed state before rethrowing, including async exceptions.
            pure (False, Left exception)
    either throwIO (either throwIO pure) outcome

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` (\e -> const (pure ()) (e :: IOException))
