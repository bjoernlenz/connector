-- This must fail to typecheck: the ordinary write API requires WriteContext.
module MissingContext where
import Datomic.Connector
import Datomic.EDN

anonymousWrite :: Connector -> IO EDN
anonymousWrite c = transact c (Vector [])
