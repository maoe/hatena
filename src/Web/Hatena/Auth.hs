module Web.Hatena.Auth
  ( HatenaAuth(authenticate)
  , module Web.Hatena.Auth.OAuth
  , module Web.Hatena.Auth.WSSE
  ) where

import Control.Monad.Trans (MonadTrans(..))

import Control.Monad.Trans.Resource (ResourceIO)
import Network.HTTP.Conduit (Request)
import Web.Authenticate.OAuth (signOAuth)

import Web.Hatena.Auth.OAuth
import Web.Hatena.Auth.WSSE
import Web.Hatena.Monad

class HatenaAuth auth where
  authenticate :: ResourceIO m => Request m -> HatenaT auth m (Request m)

instance HatenaAuth (OAuth scope) where
  authenticate req = do
    auth <- getAuth
    lift $ signOAuth (oAuthConsumer auth)
                     (oAuthCredentials auth)
                     req

instance HatenaAuth WSSE where
  authenticate = undefined
