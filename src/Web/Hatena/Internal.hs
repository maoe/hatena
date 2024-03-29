{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Hatena.Internal
  ( postAtom, putAtom, deleteAtom
  , emptyResponse
  , jsonResponse, jsonValueResponse
  , atomResponse
  , FromAtom(..)
  , camelToSnake
  ) where
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Data.Char (toLower, isUpper)
import Data.List (stripPrefix)

import Control.Failure (Failure)
import Data.Aeson (Value, json)
import Data.Conduit (ResourceIO, ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Conduit
import Text.XML (Document(..), Prologue(..), Element(..), Node, sinkDoc, renderLBS)
import qualified Data.Aeson.Parser as JSON (value')

import Web.Hatena.Monad (HatenaT, getManager)

postAtom :: (ResourceIO m, Failure HttpException m)
         => String -> [Node] -> HatenaT auth m (Request m)
postAtom uri nodes = do
  req <- requestAtom uri nodes
  return $ req { method = "POST" }

putAtom :: (ResourceIO m, Failure HttpException m)
        => String -> [Node] -> HatenaT auth m (Request m)
putAtom uri nodes = do
  req <- requestAtom uri nodes
  return $ req { method = "PUT" }

deleteAtom :: (ResourceIO m, Failure HttpException m)
           => String -> [Node] -> HatenaT auth m (Request m)
deleteAtom uri nodes = do
  req <- requestAtom uri nodes
  return $ req { method = "DELETE" }

requestAtom :: (ResourceIO m, Failure HttpException m)
         => String -> [Node] -> HatenaT auth m (Request m)
requestAtom uri nodes = do
  req <- lift $ parseUrl uri
  return req
    { requestBody = RequestBodyLBS $ renderLBS def doc
    }
  where
    doc = Document (Prologue [] Nothing []) root []
    root = Element "entry" [("xmlns", "http://purl.org/atom/ns#")] nodes

emptyResponse :: ResourceIO m => Request m -> HatenaT auth m ()
emptyResponse req = do
  manager <- getManager
  void $ lift $ http req manager

jsonResponse :: ResourceIO m => Request m -> HatenaT auth m Value
jsonResponse req = do
  manager <- getManager
  lift $ do
    resp <- http req manager
    responseBody resp $$ sinkParser json

jsonValueResponse :: ResourceIO m => Request m -> HatenaT auth m Value
jsonValueResponse req = do
  manager <- getManager
  lift $ do
    resp <- http req manager
    responseBody resp $$ sinkParser JSON.value'

atomResponse :: ResourceIO m => Request m -> HatenaT auth m Document
atomResponse req = do
  manager <- getManager
  lift $ do
    resp <- http req manager
    responseBody resp $$ sinkDoc def

class FromAtom a where
  fromAtom :: Document -> Maybe a

camelToSnake :: String -> String -> String
camelToSnake prefix xs =
  case prefix `stripPrefix` xs of
    Just ""     -> toSnake xs
    Just (y:ys) -> toLower y : toSnake ys
    Nothing     -> camelToSnake "" xs
  where
    toSnake = foldr f ""
    f y ys
      | isUpper y = '_':toLower y:ys
      | otherwise = y:ys
