{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Hatena.Monad
  ( HatenaT
  , newHatenaEnv
  , runHatenaT
  , getAuth, getManager
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, asks)
import Control.Monad.Trans (MonadTrans, MonadIO)

import Control.Monad.Trans.Resource (ResourceT)
import Network.HTTP.Conduit

newtype Hatena auth m a = Hatena
  { runHatena :: ReaderT (HatenaEnv auth) m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (HatenaEnv auth)
           , MonadTrans
           , MonadIO
           )

type HatenaT auth m = Hatena auth (ResourceT m)

runHatenaT :: auth -> HatenaT auth m a -> Manager -> ResourceT m a
runHatenaT auth act manager =
  runReaderT (runHatena act) $ newHatenaEnv auth manager

getAuth :: Monad m => Hatena auth m auth
getAuth = asks hatenaAuth

getManager :: Monad m => Hatena auth m Manager
getManager = asks hatenaManager

data HatenaEnv auth = HatenaEnv
  { hatenaAuth    :: auth
  , hatenaManager :: Manager
  }

newHatenaEnv :: auth -> Manager -> HatenaEnv auth
newHatenaEnv = HatenaEnv
