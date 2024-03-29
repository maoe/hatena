{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Hatena.Bookmark.Atom
  ( getPostEndPoint
  , PostEndPoint(..)
  , postBookmark
  , getBookmark
  , editBookmark
  , deleteBookmark
  , EditEndPoint(..)
  , getFeedEndPoint
  , FeedEndPoint(..)
  , getFeed
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
import Text.XML (Document)
import Text.XML.Cursor (($//), (>=>), fromDocument, attributeIs, attribute)

import Web.Hatena.Auth
import Web.Hatena.Monad
import Web.Hatena.Internal

getPostEndPoint :: (OAuthRead scope, ResourceIO m, Failure HttpException m)
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

postBookmark
  :: (OAuthWrite scope, ResourceIO m, Failure HttpException m)
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

getBookmark :: (OAuthRead scope, ResourceIO m, Failure HttpException m)
            => EditEndPoint
            -> HatenaT (Either (OAuth scope) WSSE) m Document
getBookmark (EditEndPoint url) = do
  req <- lift $ parseUrl $ T.unpack url
  req' <- authenticate req
  atomResponse req'  

editBookmark :: (OAuthWrite scope, ResourceIO m, Failure HttpException m)
             => EditEndPoint
             -> Maybe Text -- ^ Title
             -> Maybe Text -- ^ Comment
             -> HatenaT (Either (OAuth scope) WSSE) m ()
editBookmark _                  Nothing Nothing   = fail "failed"
editBookmark (EditEndPoint url) title'm summary'm = do
  req <- putAtom (T.unpack url)
                 [xml|
                   $maybe title <- title'm
                     <title>#{title}
                   $maybe summary <- summary'm
                     <summary>#{summary}
                 |]
  req' <- authenticate req
  emptyResponse req'

deleteBookmark :: (OAuthWrite scope, ResourceIO m, Failure HttpException m)
               => EditEndPoint
               -> HatenaT (Either (OAuth scope) WSSE) m ()
deleteBookmark (EditEndPoint url) = do
  req <- deleteAtom (T.unpack url) []
  req' <- authenticate req
  emptyResponse req'

getFeedEndPoint :: (OAuthRead scope, ResourceIO m, Failure HttpException m)
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

getFeed :: (OAuthRead scope, ResourceIO m, Failure HttpException m)
        => HatenaT (Either (OAuth scope) WSSE) m Document
getFeed = do
  FeedEndPoint url <- getFeedEndPoint
  req <- lift $ parseUrl $ T.unpack url
  req' <- authenticate req
  atomResponse req'
