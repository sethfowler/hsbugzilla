{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Internal
( BzServer
, BzContext (..)
, BugzillaException (..)
, bzRequest
, requestUrl
, QueryPart
) where

import Blaze.ByteString.Builder (toByteString)
import Control.Exception (Exception)
import qualified Data.ByteString as B
import Data.Default (def)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Network.HTTP.Conduit (Manager, Request(..), host, path, queryString, secure)
import Network.HTTP.Types.URI (QueryText, encodePathSegments, renderQueryText)

type BzServer  = T.Text

data BzContext = BzContext
  { bzServer  :: BzServer
  , bzManager :: Manager
  }

data BugzillaException = BugzillaException String
  deriving (Show, Typeable)
instance Exception BugzillaException

sslRequest :: Request
sslRequest =
  def {
    secure = True,
    port   = 443
  }

bzRequest :: BzContext -> [T.Text] -> QueryText -> Request
bzRequest ctx methodParts query =
  sslRequest {
    host        = TE.encodeUtf8 $ bzServer ctx,
    path        = toByteString $ encodePathSegments $ "rest" : methodParts,
    queryString = toByteString $ renderQueryText True query
  }

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

type QueryPart = (T.Text, Maybe T.Text)
