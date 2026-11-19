-- | Connector exceptions intentionally do not inherit IOException.
module Datomic.Error (ConnectorError(..)) where

import Control.Exception (Exception, SomeException)
import Datomic.EDN (EDN)

data ConnectorError
  = LocalValidation String
  | DatomicRejected EDN -- ^ Complete bridge rejection, including class/message/ex-data.
  | StartupFailure String
  | ProtocolFailure String
  | ConnectorClosed
  | WriteOutcomeUnknown SomeException -- ^ Reconcile before retrying; retains the cause.
  deriving Show

instance Exception ConnectorError
