module Starter.Migrations (Migration, loadMigration, runMigrations) where

import Control.Exception (throwIO, try)
import Control.Monad (unless)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)
import Datomic.Connector
import Datomic.EDN
import Starter.Persistence (resolveContext)

-- Hash the exact UTF-8 file bytes, including whitespace. Applied files are immutable.
data Migration = Migration String String [EDN]

loadMigration :: String -> FilePath -> IO Migration
loadMigration ident path = do
  bytes <- BS.readFile path
  text <- either (throwIO . LocalValidation . show) pure (Text.decodeUtf8' bytes)
  tx <- either (throwIO . LocalValidation) pure (parseEDN (Text.unpack text))
  entries <- case tx of Vector xs -> pure xs; _ -> throwIO (LocalValidation "Migration must be an EDN vector")
  unless (not (null ident) && not (any reserved entries))
    (throwIO (LocalValidation "Migration ID is empty or content modifies runner bookkeeping"))
  pure (Migration ident (concatMap hex (BS.unpack (SHA256.hash bytes))) entries)
  where
    hex n = let s = showHex n "" in if length s == 1 then '0':s else s
    reserved (Keyword k) = take 10 k == "migration/" || k == "project/migrations"
    reserved (Vector xs) = any reserved xs
    reserved (List xs) = any reserved xs
    reserved (Set xs) = any reserved xs
    reserved (Map xs) = any (\(k,v) -> reserved k || reserved v) xs
    reserved (Tagged _ x) = reserved x
    reserved _ = False

runMigrations :: Connector -> [Migration] -> IO ()
runMigrations c migrations = do
  context <- resolveContext c
  mapM_ (apply context 32) migrations
  where
    apply :: WriteContext -> Int -> Migration -> IO ()
    apply context remaining migration@(Migration ident checksum entries) = do
      (applied, revision) <- status ident
      case applied of
        Just old | old == checksum -> pure ()
                 | otherwise -> throwIO (LocalValidation ("Checksum mismatch for applied migration: " ++ ident))
        Nothing -> do
          unless (remaining > 0) (throwIO (LocalValidation "Migration contention limit reached; rerun after other runners finish"))
          let tx = Vector (entries ++
                [ Vector [Keyword "db/cas", Keyword "project/migrations", Keyword "migration/revision", IntEDN revision, IntEDN (revision + 1)]
                , Map [(Keyword "migration/id", StringEDN ident), (Keyword "migration/checksum", StringEDN checksum)] ])
          result <- try (transactReport c context tx)
          case result of
            Right _ -> pure ()
            Left rejection@(DatomicRejected _) -> do
              -- Only complete rejections are safe to reconsider. Never catch a
              -- lost write response, cancellation or a transport failure here.
              (nowApplied, nowRevision) <- status ident
              if nowApplied /= Nothing || nowRevision /= revision
                then apply context (remaining - 1) migration
                else throwIO rejection
            Left other -> throwIO other
    status ident = withSnapshot c $ \snapshot -> do
      record <- pullSnapshot snapshot (Vector [Keyword "migration/checksum"])
        (Vector [Keyword "migration/id", StringEDN ident])
      state <- pullSnapshot snapshot (Vector [Keyword "migration/revision"]) (Keyword "project/migrations")
      revision <- case field "migration/revision" state of
        Just (IntEDN n) | n >= 0 -> pure n
        _ -> throwIO (LocalValidation "Missing migration state; initialize/adopt the project schema explicitly")
      applied <- case field "migration/checksum" record of
        Nothing -> pure Nothing
        Just (StringEDN checksum) -> pure (Just checksum)
        _ -> throwIO (ProtocolFailure "Invalid migration checksum record")
      pure (applied, revision)
