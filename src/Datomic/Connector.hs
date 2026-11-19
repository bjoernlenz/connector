{-# LANGUAGE ScopedTypeVariables #-}
module Datomic.Connector
  (Connector, ConnectorError(..), EntityId(..), WriteContext(..), withConnector,
   LocalConfig(..), defaultLocalConfig, withLocalConnector, transact, transactReport, query, pull,
   Snapshot, withSnapshot, snapshotBasis, querySnapshot, pullSnapshot,
   Basis(..), TempId(..), Datom(..), TransactionReport(..)) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, SomeAsyncException, IOException, mask, try, throwIO, catch, toException, fromException)
import Control.Monad (void)
import Datomic.Error
import Datomic.EDN
import Datomic.Report
import Datomic.Runtime
import Data.Version (showVersion)
import Paths_connector (version)
import System.Directory (makeAbsolute)
import System.Exit (ExitCode(..))
import System.IO
import System.Process hiding (system)
import System.Timeout (timeout)

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
  process <- startup $ do
    storage <- makeAbsolute (storageDirectory config)
    bridgeProcess (bridgeRuntime config) ["serve", storage, systemName config, databaseName config]
  withConnectorProcess process $ \connector -> do
    ready <- (timeout (startupTimeoutMicros config) (request connector [("op", Keyword "hello")]))
      `catch` \(e :: ConnectorError) -> throwIO (StartupFailure (show e))
    case ready of
      Just info | field "protocol" info == Just (IntEDN 1)
               && field "version" info == Just (StringEDN (showVersion version)) -> action connector
      Nothing -> throwIO (StartupFailure "Bridge startup timed out")
      _ -> throwIO (StartupFailure "Bridge protocol/package version mismatch")

-- The command must launch the bridge in serve mode; stderr stays visible.
withConnector :: FilePath -> [String] -> (Connector -> IO a) -> IO a
withConnector command args = withConnectorProcess (proc command args)

withConnectorProcess :: CreateProcess -> (Connector -> IO a) -> IO a
withConnectorProcess launch action = mask $ \restore -> do
  connector <- startup start
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
      either (\e -> tryAny (stop connector) >> throwIO e) (const (pure connector)) setup
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
        Just exit | healthy -> throwIO (ProtocolFailure ("Bridge exited: " ++ show exit))
        Nothing | healthy -> throwIO (ProtocolFailure "Bridge did not shut down within two seconds")
        _ -> pure ()

transact :: Connector -> WriteContext -> EDN -> IO EDN
transact c context tx = transactWith (\v -> decodeTransactionReport v >> Right v) c context tx

transactReport :: Connector -> WriteContext -> EDN -> IO TransactionReport
transactReport = transactWith decodeTransactionReport

transactWith :: (EDN -> Either String a) -> Connector -> WriteContext -> EDN -> IO a
transactWith decode c (WriteContext (EntityId a) (EntityId v)) tx
  | a <= 0 || v <= 0 = throwIO (LocalValidation "WriteContext requires positive entity IDs")
  | otherwise = requestWith decode c
      [("op", Keyword "transact"), ("context", Map [(Keyword "actor", IntEDN a), (Keyword "via", IntEDN v)]),
       ("tx-data", tx)]

-- The bridge supplies the current database as the first query input ($).
query :: Connector -> EDN -> [EDN] -> IO EDN
query c q inputs = request c [("op", Keyword "query"), ("query", q), ("args", Vector inputs)]

pull :: Connector -> EDN -> EDN -> IO EDN
pull c selector eid = request c [("op", Keyword "pull"), ("selector", selector), ("eid", eid)]

-- | A handle owned by one connector and valid only inside withSnapshot.
data Snapshot = Snapshot Connector String Basis (MVar Bool)

snapshotBasis :: Snapshot -> Basis
snapshotBasis (Snapshot _ _ basis _) = basis

withSnapshot :: Connector -> (Snapshot -> IO a) -> IO a
withSnapshot c action = mask $ \restore -> do
  (token, basis) <- requestWith decode c [("op", Keyword "snapshot/open")]
  alive <- newMVar True
  let snapshot = Snapshot c token basis alive
  result <- tryAny (restore (action snapshot))
  cleanup <- tryAny $ modifyMVar_ alive $ \_ -> do
    _ <- request c [("op", Keyword "snapshot/close"), ("snapshot", StringEDN token)]
      `catch` \e -> case e of ConnectorClosed -> pure Nil; _ -> throwIO e
    pure False
  either throwIO (\value -> either throwIO (const (pure value)) cleanup) result
  where
    decode v = case (field "snapshot" v, field "t" v) of
      (Just (StringEDN token), Just (IntEDN t)) | t >= 0 -> Right (token, Basis t)
      _ -> Left "Malformed snapshot response"

querySnapshot :: Snapshot -> EDN -> [EDN] -> IO EDN
querySnapshot snapshot q inputs = snapshotRequest snapshot
  [("op", Keyword "query"), ("query", q), ("args", Vector inputs)]

pullSnapshot :: Snapshot -> EDN -> EDN -> IO EDN
pullSnapshot snapshot selector eid = snapshotRequest snapshot
  [("op", Keyword "pull"), ("selector", selector), ("eid", eid)]

snapshotRequest :: Snapshot -> [(String, EDN)] -> IO EDN
snapshotRequest (Snapshot c token _ alive) entries = withMVar alive $ \open ->
  if open then request c (("snapshot", StringEDN token) : entries)
  else throwIO (LocalValidation "Snapshot scope has ended")

-- Private: callers cannot send arbitrary protocol operations or omit context.
request :: Connector -> [(String, EDN)] -> IO EDN
request = requestWith Right

requestWith :: (EDN -> Either String a) -> Connector -> [(String, EDN)] -> IO a
requestWith decode (Connector input output process lock) entries = do
  -- Validate the entire value before writing even one byte to the stream.
  encoded <- either (throwIO . LocalValidation) pure (encodeEDN (Map [(Keyword k,v) | (k,v) <- entries]))
  exchange encoded
 where
  exchange encoded = mask $ \restore -> do
    outcome <- modifyMVar lock $ \healthy ->
      if not healthy then pure (False, Left (toException ConnectorClosed))
      else do
        result <- tryAny $ restore $ do
          hPutStrLn input encoded
          line <- hGetLine output
          response <- either (throwIO . ProtocolFailure) pure (parseEDN line)
          envelope <- either (throwIO . ProtocolFailure) pure (decodeEnvelope response)
          case envelope of
            Right value -> Right <$> either (throwIO . ProtocolFailure) pure (decode value)
            Left err -> pure (Left (DatomicRejected err))
        case result of
          Right response -> pure (True, Right response)
          Left exception -> do
            _ <- tryAny (terminateProcess process)
            -- Commit the closed state before rethrowing, including async exceptions.
            pure (False, Left (exchangeFailure exception))
    either throwIO (either throwIO pure) outcome
  exchangeFailure exception
    | Just (_ :: SomeAsyncException) <- fromException exception = exception
    | lookup "op" entries == Just (Keyword "transact") = toException (WriteOutcomeUnknown exception)
    | Just (_ :: ConnectorError) <- fromException exception = exception
    | otherwise = toException (ProtocolFailure (show exception))

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` (\e -> const (pure ()) (e :: IOException))

startup :: IO a -> IO a
startup action = action `catch` \(e :: IOException) -> throwIO (StartupFailure (show e))

-- Protocol v1 has exactly one result or one structured error. A malformed
-- rejection cannot be treated as a known, complete Datomic refusal.
decodeEnvelope :: EDN -> Either String (Either EDN EDN)
decodeEnvelope response = case response of
  Map fields | length fields == 2 -> case (field "ok" response, field "result" response, field "error" response) of
    (Just (Boolean True), Just value, Nothing) -> Right (Right value)
    (Just (Boolean False), Nothing, Just err@(Map _))
      | Just (StringEDN _) <- field "class" err
      , Just message <- field "message" err
      , stringOrNil message
      , Just details <- field "data" err
      , mapOrNil details -> Right (Left err)
    _ -> Left "Malformed bridge response"
  _ -> Left "Malformed bridge envelope"
  where
    stringOrNil (StringEDN _) = True
    stringOrNil Nil = True
    stringOrNil _ = False
    mapOrNil (Map _) = True
    mapOrNil Nil = True
    mapOrNil _ = False
