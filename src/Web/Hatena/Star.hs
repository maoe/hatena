{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Hatena.Star where
import Control.Applicative ((<*>))
import Control.Monad.Trans (lift)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS (unpack)

import Control.Failure (Failure)
import Control.Monad.Trans.Resource (ResourceIO)
import Data.Conduit (($$))
import Network.HTTP.Conduit
import Network.HTTP.Types (Ascii)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A (deriveJSON)

import Web.Hatena.Monad
import Web.Hatena.Internal (jsonResponse, camelToSnake)

data StarCount = StarCount
  { starCountTitle :: Text
  , starCount      :: Int
  , starCountUri   :: Ascii
  , starCountCount :: A.Object
  } deriving Show

$(A.deriveJSON (camelToSnake "starCount") ''StarCount)

getStarCount :: (ResourceIO m, Failure HttpException m)
             => Ascii -> HatenaT noauth m StarCount
getStarCount uri = do
  req <- lift $ parseUrl $ "http://s.hatena.ne.jp/blog.json/" ++ BS.unpack uri
  json <- jsonResponse req
  case A.fromJSON json of
    A.Success starCount -> return starCount
    A.Error reason      -> fail reason
