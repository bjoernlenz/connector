-- | Validated transaction reports. Datom values remain lossless EDN scalars.
module Datomic.Report
  (EntityId(..), Basis(..), TempId(..), Datom(..), TransactionReport(..), decodeTransactionReport) where

import Control.Monad (unless)
import qualified Data.Set as Set
import Datomic.EDN

newtype EntityId = EntityId Integer deriving (Eq, Ord, Show)
newtype Basis = Basis Integer deriving (Eq, Ord, Show)
data TempId = StringTempId String | IntTempId Integer deriving (Eq, Ord, Show)
data Datom = Datom
  { datomEntity :: EntityId
  , datomAttribute :: EntityId
  , datomValue :: EDN
  , datomTransaction :: EntityId
  , datomAdded :: Bool
  } deriving (Eq, Show)
data TransactionReport = TransactionReport
  { transactionBasis :: Basis
  , transactionEntity :: EntityId
  , transactionTempids :: [(TempId, EntityId)]
  , transactionDatoms :: [Datom]
  } deriving (Eq, Show)

decodeTransactionReport :: EDN -> Either String TransactionReport
decodeTransactionReport value = do
  fields <- case value of Map xs -> Right xs; _ -> Left "Report must be a map"
  unless (length fields == 4 && all (\k -> length (filter ((== Keyword k) . fst) fields) == 1)
    ["t", "tx", "tempids", "tx-data"]) (Left "Report requires exactly t, tx, tempids and tx-data")
  basis <- case field "t" value of Just (IntEDN t) | t >= 0 -> Right (Basis t); _ -> Left "Invalid transaction basis"
  tx <- required "tx" >>= entity
  ids <- required "tempids" >>= \v -> case v of
    Map xs -> mapM tempid xs
    _ -> Left "tempids must be a map"
  unless (length ids == Set.size (Set.fromList (map fst ids))) (Left "Duplicate tempid")
  ds <- required "tx-data" >>= \v -> case v of
    Vector xs -> mapM datom xs
    _ -> Left "tx-data must be a vector"
  unless (not (null ds) && all ((== tx) . datomTransaction) ds) (Left "Missing or inconsistent transaction datoms")
  case lookup (StringTempId "datomic.tx") ids of
    Just t | t /= tx -> Left "Inconsistent transaction tempid"
    _ -> Right (TransactionReport basis tx ids ds)
  where
    required k = maybe (Left ("Missing " ++ k)) Right (field k value)
    entity (IntEDN n) | n > 0 = Right (EntityId n)
    entity _ = Left "Expected positive entity ID"
    tempid (StringEDN k, v) = (,) (StringTempId k) <$> entity v
    tempid (IntEDN k, v) | k < 0 = (,) (IntTempId k) <$> entity v
    tempid _ = Left "Invalid tempid mapping"
    datom (Vector [e,a,v,t,Boolean added]) = do
      unless (scalar v) (Left "Invalid datom scalar")
      Datom <$> datomEntityId e <*> entity a <*> pure v <*> entity t <*> pure added
    datom _ = Left "Datom requires [entity attribute value transaction added]"
    datomEntityId (IntEDN n) | n >= 0 = Right (EntityId n)
    datomEntityId _ = Left "Expected nonnegative datom entity ID"
    scalar (Map _) = False
    scalar (List _) = False
    scalar (Set _) = False
    scalar Nil = False
    scalar (Tagged tag (StringEDN _)) = tag `elem` ["inst", "uuid", "connector/float", "connector/uri", "connector/bytes"]
    scalar (Tagged _ _) = False
    scalar (Vector xs) = all (\x -> x == Nil || scalar x) xs -- Partial composite tuple
    scalar _ = True
