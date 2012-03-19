{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Hatena.Bookmark
  ( getPostEndPoint
  , PostEndPoint(..)
  , getFeedEndPoint
  , FeedEndPoint(..)
  , postBookmark
  , EditEndPoint(..)
  ) where
import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Failure (Failure)
import Control.Monad.Trans.Resource (ResourceIO)
import Network.HTTP.Conduit (HttpException, parseUrl)
import Text.Hamlet.XML (xml)
import Text.XML.Cursor (($//), (>=>), fromDocument, attributeIs, attribute)

import Web.Hatena.Auth
import Web.Hatena.Monad
import Web.Hatena.Internal (FromAtom(..), postAtom, atomResponse)

getPostEndPoint
  :: (ResourceIO m, Failure HttpException m)
  => HatenaT (Either (OAuth scope) WSSE) m PostEndPoint
getPostEndPoint = do
  req <- lift $ parseUrl "http://b.hatena.ne.jp/atom"
  req' <- authenticate req
  atom <- atomResponse req'
  case fromAtom atom of
    Just endPoint -> return endPoint
    Nothing       -> fail "failed"

newtype PostEndPoint = PostEndPoint { postEndPoint :: Text }
  deriving Show

instance FromAtom PostEndPoint where
  fromAtom doc = PostEndPoint <$> listToMaybe href
    where
      href = fromDocument doc $// "rel" `attributeIs` "service.post"
                              >=> attribute "href"

getFeedEndPoint
  :: (ResourceIO m, Failure HttpException m)
  => HatenaT (Either (OAuth scope) WSSE) m FeedEndPoint
getFeedEndPoint = do
  req <- lift $ parseUrl "http://b.hatena.ne.jp/atom"
  req' <- authenticate req
  atom <- atomResponse req'
  case fromAtom atom of
    Just endPoint -> return endPoint
    Nothing       -> fail "failed"

newtype FeedEndPoint = FeedEndPoint { feedEndPoint :: Text }
  deriving Show

instance FromAtom FeedEndPoint where
  fromAtom doc = FeedEndPoint <$> listToMaybe href
    where
      href = fromDocument doc $// "rel" `attributeIs` "service.feed"
                              >=> attribute "href"

postBookmark
  :: (ResourceIO m, Failure HttpException m)
  => Text -- ^ URI
  -> Text -- ^ Comments
  -> HatenaT (Either (OAuth scope) WSSE) m EditEndPoint
postBookmark uri summary = do
  post <- getPostEndPoint
  req <- postAtom (T.unpack (postEndPoint post))
                  [xml|
                    <link rel=related type=text/html href=#{uri}>
                    <summary type=text/plain>#{summary}
                  |]
  req' <- authenticate req
  atom <- atomResponse req'
  case fromAtom atom of
    Just endPoint -> return endPoint
    Nothing       -> fail "failed"

newtype EditEndPoint = EditEndPoint { editEndPoint :: Text }
  deriving Show

instance FromAtom EditEndPoint where
  fromAtom doc = EditEndPoint <$> listToMaybe href
    where
      href = fromDocument doc $// "rel" `attributeIs` "service.edit"
                              >=> attribute "href"
