module Web.Hatena.Auth.WSSE
  ( WSSE(wsseCredentials)
  ) where

import Data.Text (Text)

data WSSE = WSSE
  { wsseCredentials :: Text
  } deriving Show
