{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Hatena.OAuth
  ( getUserInfo
  , startApplication
  ) where
import Control.Applicative ((<*>))
import Control.Monad.Trans (lift)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Control.Failure (Failure)
import Control.Monad.Trans.Resource (ResourceIO)
import Data.Conduit (($$))
import Data.Conduit.Attoparsec
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A (deriveJSON)

import Web.Hatena.Auth
import Web.Hatena.Monad

data UserInfo = UserInfo
  { userInfoUrlName         :: Ascii
  , userInfoDisplayName     :: Text
  , userInfoProfileImageUri :: Ascii
  } deriving Show

$(A.deriveJSON (fromMaybe <*> stripPrefix "userInfo") ''UserInfo)

getUserInfo :: (OAuthReadPublic scope, ResourceIO m, Failure HttpException m)
            => HatenaT (OAuth scope) m UserInfo
getUserInfo = do
  req <- lift $ parseUrl "http://n.hatena.com/applications/my.json"
  req' <- authenticate req
  manager <- getManager
  json <- lift $ do
    resp <- http req' manager
    responseBody resp $$ sinkParser A.json
  case A.fromJSON json of
    A.Success userInfo -> return userInfo
    A.Error reason     -> fail reason

newtype StartApplication = StartApplication Int

$(A.deriveJSON id ''StartApplication)

startApplication :: (OAuthWritePublic scope, ResourceIO m, Failure HttpException m)
                 => HatenaT (OAuth scope) m StartApplication
startApplication = do
  req <- lift $ parseUrl "http://n.hatena.com/applications/start"
  req' <- authenticate req
  manager <- getManager
  json <- lift $ do
    resp <- http req' manager
    responseBody resp $$ sinkParser A.json
  case A.fromJSON json of
    A.Success start -> return start
    A.Error reason  -> fail reason
