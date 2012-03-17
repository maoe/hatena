module Web.Hatena.Internal
  ( jsonResponse
  , atomResponse
  , camelToSnake
  ) where
import Control.Monad.Trans (lift)
import Data.Char
import Data.List (stripPrefix)

import Data.Conduit (ResourceIO, ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Conduit (Request, http, responseBody)
import Data.Aeson (Value, json)
import Text.XML (Document, sinkDoc, def)

import Web.Hatena.Monad (HatenaT, getManager)

jsonResponse :: ResourceIO m => Request m -> HatenaT auth m Value
jsonResponse req = do
  manager <- getManager
  lift $ do
    resp <- http req manager
    responseBody resp $$ sinkParser json

atomResponse :: ResourceIO m => Request m -> HatenaT auth m Document
atomResponse req = do
  manager <- getManager
  lift $ do
    resp <- http req manager
    responseBody resp $$ sinkDoc def

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
