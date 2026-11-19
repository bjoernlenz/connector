module Starter.Persistence (Note(..), resolveContext, saveNote, findNote) where

import Control.Exception (throwIO)
import Datomic.Connector
import Datomic.EDN

-- Project types and schema names stay outside the connector.
data Note = Note { noteId :: EntityId, noteKey :: String, noteText :: String }
  deriving (Eq, Show)

resolveContext :: Connector -> IO WriteContext
resolveContext c = WriteContext <$> identity "project/actor" <*> identity "project/application"
  where
    identity name = do
      value <- pull c (Vector [Keyword "db/id"]) (Keyword name)
      case field "db/id" value of
        Just (IntEDN n) | n > 0 -> pure (EntityId n)
        _ -> throwIO (LocalValidation ("Missing project identity: " ++ name))

saveNote :: Connector -> WriteContext -> String -> String -> IO EntityId
saveNote c context key text = do
  report <- transactReport c context (Vector [Map
    [(Keyword "db/id", StringEDN "note"), (Keyword "note/key", StringEDN key),
     (Keyword "note/text", StringEDN text)]])
  case lookup (StringTempId "note") (transactionTempids report) of
    Just eid -> pure eid
    Nothing -> throwIO (ProtocolFailure "Missing note tempid in report")

findNote :: Connector -> String -> IO (Maybe Note)
findNote c key = do
  value <- pull c (Vector [Keyword "db/id", Keyword "note/key", Keyword "note/text"])
    (Vector [Keyword "note/key", StringEDN key])
  case (field "db/id" value, field "note/key" value, field "note/text" value) of
    (Just (IntEDN n), Just (StringEDN k), Just (StringEDN text)) -> pure (Just (Note (EntityId n) k text))
    (Just Nil, Nothing, Nothing) -> pure Nothing
    _ -> throwIO (ProtocolFailure ("Invalid note: " ++ show value))
