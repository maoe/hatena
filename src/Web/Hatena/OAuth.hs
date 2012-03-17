{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Hatena.OAuth
  ( UserInfo(..), getUserInfo
  , StartApplication(..), startApplication
  ) where
import Control.Monad.Trans (lift)
import Data.Text (Text)

import Control.Failure (Failure)
import Control.Monad.Trans.Resource (ResourceIO)
import Network.HTTP.Conduit (HttpException, parseUrl)
import Network.HTTP.Types (Ascii)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A (deriveJSON)

import Web.Hatena.Auth
import Web.Hatena.Monad
import Web.Hatena.Internal (jsonResponse, camelToSnake)

data UserInfo = UserInfo
  { userInfoUrlName         :: Ascii
  , userInfoDisplayName     :: Text
  , userInfoProfileImageUrl :: Ascii
  } deriving Show

$(A.deriveJSON (camelToSnake "userInfo") ''UserInfo)

getUserInfo :: (OAuthReadPublic scope, ResourceIO m, Failure HttpException m)
            => HatenaT (OAuth scope) m UserInfo
getUserInfo = do
  req <- lift $ parseUrl "http://n.hatena.com/applications/my.json"
  req' <- authenticate req
  json <- jsonResponse req'
  case A.fromJSON json of
    A.Success userInfo -> return userInfo
    A.Error reason     -> fail reason

newtype StartApplication = StartApplication
  { startApplicationResult :: Int
  } deriving Show

$(A.deriveJSON (camelToSnake "startApplication") ''StartApplication)

startApplication :: (OAuthWritePublic scope, ResourceIO m, Failure HttpException m)
                 => HatenaT (OAuth scope) m StartApplication
startApplication = do
  req <- lift $ parseUrl "http://n.hatena.com/applications/start"
  req' <- authenticate req
  json <- jsonResponse req'
  case A.fromJSON json of
    A.Success start -> return start
    A.Error reason  -> fail reason
