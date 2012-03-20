{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Web.Hatena.Bookmark.Entry where
import Control.Monad.Trans (lift)
import qualified Data.ByteString.Char8 as B8

import Control.Failure (Failure)
import Control.Monad.Trans.Resource (ResourceIO)
import Network.HTTP.Types (urlEncode)
import Network.HTTP.Conduit (HttpException, parseUrl)
import qualified Data.Aeson as A

import Web.Hatena.Monad
import Web.Hatena.Internal

getCount :: (ResourceIO m, Failure HttpException m)
         => String -> HatenaT noauth m Integer
getCount url = do
  req <- lift $ parseUrl $ "http://api.b.st-hatena.com/entry.count?url=" ++
                           B8.unpack (urlEncode True (B8.pack url))
  json <- jsonValueResponse req
  case A.fromJSON json of
    A.Success count -> return count
    A.Error reason -> fail reason
