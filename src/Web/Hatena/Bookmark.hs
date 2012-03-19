{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Hatena.Bookmark
  ( getRootAtomEndPoints
  , AtomEndPoint(..)
  , postBookmark
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
import Web.Hatena.Internal (postAtom, atomResponse)

getRootAtomEndPoints
  :: (ResourceIO m, Failure HttpException m)
  => HatenaT (Either (OAuth scope) WSSE) m (AtomEndPoint, AtomEndPoint)
getRootAtomEndPoints = do
  req <- lift $ parseUrl "http://b.hatena.ne.jp/atom"
  req' <- authenticate req
  atom <- atomResponse req'
  case fromAtom atom of
    Just endPoints -> return endPoints
    Nothing        -> fail "failed"

fromAtom :: Document -> Maybe (AtomEndPoint, AtomEndPoint)
fromAtom atom = listToMaybe $ zip (AtomEndPoint <$> postEndPoints)
                                  (AtomEndPoint <$> feedEndPoints)
  where
    cursor = fromDocument atom
    postEndPoints = cursor $// "rel" `attributeIs` "service.post"
                           >=> attribute "href"
    feedEndPoints = cursor $// "rel" `attributeIs` "service.feed"
                           >=> attribute "href"

newtype AtomEndPoint = AtomEndPoint { atomEndPoint :: Text }
  deriving Show

postBookmark
  :: (ResourceIO m, Failure HttpException m)
  => Text -- ^ URI
  -> Text -- ^ Comments
  -> HatenaT (Either (OAuth scope) WSSE) m AtomEndPoint
postBookmark uri summary = do
  (postEndPoint, _) <- getRootAtomEndPoints
  req <- postAtom (T.unpack (atomEndPoint postEndPoint))
                  [xml|
                    <link rel=related type=text/html href=#{uri}>
                    <summary type=text/plain>#{summary}
                  |]
  req' <- authenticate req
  atom <- atomResponse req'
  case fromAtom' atom of
    Just endPoint -> return endPoint
    Nothing       -> fail "failed"

fromAtom' :: Document -> Maybe AtomEndPoint
fromAtom' atom = listToMaybe $ (AtomEndPoint <$> editEndPoint)
  where
    cursor = fromDocument atom
    editEndPoint = cursor $// "rel" `attributeIs` "service.edit"
                          >=> attribute "href"
