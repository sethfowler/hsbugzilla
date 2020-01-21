{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Internal.Network
( BugzillaServer
, BugzillaContext (..)
, BugzillaToken (..)
, BugzillaSession (..)
, BugzillaException (..)
, QueryPart
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
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Network.HTTP.Conduit (Manager, Request(..), Response(..), defaultRequest, host, httpLbs, path, queryString, secure)
import Network.HTTP.Types.URI (QueryText, encodePathSegments, renderQueryText)

type BugzillaServer  = T.Text

-- | Holds information about a 'BugzillaServer' and manages outgoing
-- connections. You can use 'newBugzillaContext' or
-- 'withBugzillaContext' to create one.
data BugzillaContext = BugzillaContext
  { bzServer  :: BugzillaServer
  , bzManager :: Manager
  }

data BugzillaToken = BugzillaToken T.Text

instance FromJSON BugzillaToken where
  parseJSON (Object v) = BugzillaToken <$> v .: "token"
  parseJSON _          = mzero

-- | A session for Bugzilla queries. Use 'anonymousSession' and
-- 'loginSession', as appropriate, to create one.
data BugzillaSession = AnonymousSession BugzillaContext
                     | LoginSession BugzillaContext BugzillaToken

bzContext :: BugzillaSession -> BugzillaContext
bzContext (AnonymousSession ctx) = ctx
bzContext (LoginSession ctx _)   = ctx

data BugzillaException
  = BugzillaJSONParseError String
  | BugzillaAPIError Int String
  | BugzillaUnexpectedValue String
    deriving (Show, Typeable)
instance Exception BugzillaException

type QueryPart = (T.Text, Maybe T.Text)

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

sslRequest :: Request
sslRequest =
  defaultRequest {
    secure = True,
    port   = 443
  }

newBzRequest :: BugzillaSession -> [T.Text] -> QueryText -> Request
newBzRequest session methodParts query =
    sslRequest {
      host        = TE.encodeUtf8 . bzServer . bzContext $ session,
      path        = toByteString $ encodePathSegments $ "rest" : methodParts,
      queryString = toByteString $ renderQueryText True queryWithToken
    }
  where
    queryWithToken = case session of
                       AnonymousSession _                   -> query
                       LoginSession _ (BugzillaToken token) -> ("token", Just token) : query

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
    Left _                   -> throw $ BugzillaJSONParseError parseError
    Right (BzError code msg) -> throw $ BugzillaAPIError code msg

sendBzRequest :: FromJSON a => BugzillaSession -> Request -> IO a
sendBzRequest session req = runResourceT $ do
  response <- liftIO $ httpLbs req . bzManager . bzContext $ session
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg      -> liftIO $ handleError msg (responseBody response)
    Right decoded -> return decoded
