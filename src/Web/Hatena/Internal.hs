module Web.Hatena.Internal
  ( jsonResponse
  , camelToSnake
  ) where
import Control.Applicative ((<*>))
import Control.Monad.Trans (lift)
import Data.Char
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

import Data.Conduit (ResourceIO, ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Conduit (Request, http, responseBody)
import Data.Aeson (Value, json)

import Web.Hatena.Monad (HatenaT, getManager)

jsonResponse :: ResourceIO m => Request m -> HatenaT auth m Value
jsonResponse req = do
  manager <- getManager
  lift $ do
    resp <- http req manager
    responseBody resp $$ sinkParser json

camelToSnake :: String -> String -> String
camelToSnake prefix xs =
  case prefix `stripPrefix` xs of
    Just ""     -> toSnake xs
    Just (y:ys) -> toLower y : toSnake ys
    Nothing     -> camelToSnake "" xs
  where
    toSnake = foldr f ""
    f x xs
      | isUpper x = '_':toLower x:xs
      | otherwise = x:xs
