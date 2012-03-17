{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Hatena.Bookmark
  ( getRootAtomEndPoints
  , AtomEndPoint(..)
  ) where
import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Control.Failure (Failure)
import Control.Monad.Trans.Resource (ResourceIO)
import Network.HTTP.Conduit (HttpException, parseUrl)
import Text.XML (Document)
import Text.XML.Cursor (($//), (>=>), fromDocument, attributeIs, attribute)

import Web.Hatena.Auth
import Web.Hatena.Monad
import Web.Hatena.Internal (atomResponse)

getRootAtomEndPoints
  :: (ResourceIO m, Failure HttpException m)
  => HatenaT noauth m (AtomEndPoint, AtomEndPoint)
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
                           >=> attribute "title"
    feedEndPoints = cursor $// "rel" `attributeIs` "service.feed"
                           >=> attribute "title"

newtype AtomEndPoint = AtomEndPoint { atomEndPoint :: Text }
  deriving Show
