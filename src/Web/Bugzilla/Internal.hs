{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Internal
( QueryPart
, BzServer
, BzContext (..)
, BugzillaException (..)
, Request
, requestUrl
, newBzRequest
, sendBzRequest
) where

import Blaze.ByteString.Builder (toByteString)
import Control.Exception (Exception, throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.ByteString as B
import Data.Default (def)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Network.HTTP.Conduit (Manager, Request(..), Response(..), host, httpLbs, path, queryString, secure)
import Network.HTTP.Types.URI (QueryText, encodePathSegments, renderQueryText)

type QueryPart = (T.Text, Maybe T.Text)
type BzServer  = T.Text

data BzContext = BzContext
  { bzServer  :: BzServer
  , bzManager :: Manager
  }

data BugzillaException = BugzillaException String
  deriving (Show, Typeable)
instance Exception BugzillaException

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

sslRequest :: Request
sslRequest =
  def {
    secure = True,
    port   = 443
  }

newBzRequest :: BzContext -> [T.Text] -> QueryText -> Request
newBzRequest ctx methodParts query =
  sslRequest {
    host        = TE.encodeUtf8 $ bzServer ctx,
    path        = toByteString $ encodePathSegments $ "rest" : methodParts,
    queryString = toByteString $ renderQueryText True query
  }

sendBzRequest :: FromJSON a => BzContext -> Request -> IO a
sendBzRequest ctx req = runResourceT $ do
  response <- liftIO $ httpLbs req (bzManager ctx)
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg      -> throw $ BugzillaException $ "JSON parse error: " ++ msg
    Right decoded -> return decoded
