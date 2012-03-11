{-# LANGUAGE EmptyDataDecls #-}
module Web.Hatena.Auth.OAuth
  ( OAuth(oAuthConsumer, oAuthCredentials)
  , newOAuth
  , ReadPublicScope, WritePublicScope, ReadPrivateScope, WritePrivateScope
  , OAuthReadPublic, OAuthWritePublic, OAuthReadPrivate, OAuthWritePrivate
  ) where
import qualified Web.Authenticate.OAuth as O

data OAuth scope = OAuth
  { oAuthConsumer    :: O.OAuth
  , oAuthCredentials :: O.Credential
  } deriving Show

newOAuth :: O.OAuth -> O.Credential -> OAuth scope
newOAuth = OAuth

class OAuthReadPublic scope
class OAuthReadPublic scope => OAuthWritePublic scope
class OAuthReadPrivate scope
class OAuthReadPrivate scope => OAuthWritePrivate scope

data ReadPublicScope
data WritePublicScope
data ReadPrivateScope
data WritePrivateScope

instance OAuthReadPublic ReadPublicScope
instance OAuthReadPublic WritePublicScope
instance OAuthWritePublic WritePublicScope
instance OAuthReadPrivate ReadPrivateScope
instance OAuthReadPrivate WritePrivateScope
instance OAuthWritePrivate WritePrivateScope
