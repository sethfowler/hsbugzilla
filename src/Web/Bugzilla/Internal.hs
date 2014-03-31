{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Internal
( QueryPart
, BzServer
, BzContext (..)
, BzToken
, BzSession (..)
, BzException (..)
, Request
, requestUrl
, newBzRequest
, sendBzRequest
) where

import Blaze.ByteString.Builder (toByteString)
import Control.Applicative
import Control.Exception (Exception, throw)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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

data BzToken = BzToken T.Text

instance FromJSON BzToken where
  parseJSON (Object v) = BzToken <$> v .: "token"
  parseJSON _          = mzero

data BzSession = AnonymousSession BzContext
               | LoginSession BzContext BzToken

bzContext :: BzSession -> BzContext
bzContext (AnonymousSession ctx) = ctx
bzContext (LoginSession ctx _)   = ctx

data BzException
  = BzJSONParseFailure String
  | BzAPIError Int String
  | BzUnexpectedValue String
    deriving (Show, Typeable)
instance Exception BzException

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

sslRequest :: Request
sslRequest =
  def {
    secure = True,
    port   = 443
  }

newBzRequest :: BzSession -> [T.Text] -> QueryText -> Request
newBzRequest session methodParts query =
    sslRequest {
      host        = TE.encodeUtf8 . bzServer . bzContext $ session,
      path        = toByteString $ encodePathSegments $ "rest" : methodParts,
      queryString = toByteString $ renderQueryText True queryWithToken
    }
  where
    queryWithToken = case session of
                       AnonymousSession _             -> query
                       LoginSession _ (BzToken token) -> ("token", Just token) : query

data BzError = BzError Int String
               deriving (Eq, Show)

instance FromJSON BzError where
  parseJSON (Object v) = BzError <$> v .: "code"
                                 <*> v .: "message"
  parseJSON _          = mzero

handleError :: String -> BL.ByteString -> IO b
handleError parseError body = do
  let mError = eitherDecode body
  case mError of
    Left _                   -> throw $ BzJSONParseFailure parseError
    Right (BzError code msg) -> throw $ BzAPIError code msg

sendBzRequest :: FromJSON a => BzSession -> Request -> IO a
sendBzRequest session req = runResourceT $ do
  response <- liftIO $ httpLbs req . bzManager . bzContext $ session
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg      -> liftIO $ handleError msg (responseBody response)
    Right decoded -> return decoded
