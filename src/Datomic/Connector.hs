module Datomic.Connector
  (Connector, EntityId(..), WriteContext(..), withConnector, transact, query, pull) where

import Control.Concurrent.MVar
import Control.Exception (bracket, finally, throwIO)
import Datomic.EDN
import System.Exit (ExitCode(..))
import System.IO
import System.Process

newtype EntityId = EntityId Integer deriving (Eq, Show)
data WriteContext = WriteContext { actor :: EntityId, via :: EntityId } deriving (Eq, Show)
data Connector = Connector Handle Handle (MVar ())

-- The command must launch the bridge in serve mode; stderr stays visible.
withConnector :: FilePath -> [String] -> (Connector -> IO a) -> IO a
withConnector command args action = bracket start stop (action . fst)
  where
    start = do
      (Just input, Just output, _, process) <- createProcess (proc command args)
        {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
      mapM_ (\h -> hSetEncoding h utf8 >> hSetBuffering h LineBuffering) [input,output]
      lock <- newMVar ()
      pure (Connector input output lock, process)
    stop (Connector input output _, process) = do
      hClose input
      code <- waitForProcess process `finally` hClose output
      case code of
        ExitSuccess -> pure ()
        _ -> throwIO (userError ("Bridge exited: " ++ show code))

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
request (Connector input output lock) entries = withMVar lock $ \_ -> do
  hPutStrLn input (renderEDN (Map [(Keyword k,v) | (k,v) <- entries]))
  line <- hGetLine output
  response <- either (throwIO . userError) pure (parseEDN line)
  case field "ok" response of
    Just (Boolean True) -> maybe (throwIO (userError "Bridge response lacks result")) pure (field "result" response)
    _ -> throwIO (userError ("Datomic bridge: " ++ renderEDN response))
