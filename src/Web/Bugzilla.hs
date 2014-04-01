{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Web.Bugzilla
( BugzillaException (..)
, BugzillaContext
, BugzillaSession (..)
, BugzillaServer
, BugzillaToken
, newBugzillaContext
, closeBugzillaContext
, withBugzillaContext
, loginSession
, anonymousSession

, BugId
, AttachmentId
, CommentId
, UserId
, FlagId
, FlagType
, UserEmail
, Field (..)
, User (..)
, Flag (..)
, Bug (..)
, Attachment (..)
, Comment (..)
, History (..)
, HistoryEntry (..)
, Change (..)
, Modification (..)
, fieldName

, searchBugs
, searchBugsWithLimit
, getBug
, getAttachment
, getAttachments
, getComments
, getHistory
) where

import Control.Exception (bracket, throw, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (mkManagerSettings, newManager, closeManager)

import Web.Bugzilla.Internal.Network
import Web.Bugzilla.Internal.Search
import Web.Bugzilla.Internal.Types

newBugzillaContext :: BugzillaServer -> IO BugzillaContext
newBugzillaContext server = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- liftIO $ newManager settings
  return $ BugzillaContext server manager

closeBugzillaContext :: BugzillaContext -> IO ()
closeBugzillaContext = closeManager . bzManager

withBugzillaContext :: BugzillaServer -> (BugzillaContext -> IO a) -> IO a
withBugzillaContext server f = bracket (newBugzillaContext server) closeBugzillaContext f

loginSession :: BugzillaContext -> UserEmail -> T.Text -> IO (Maybe BugzillaSession)
loginSession ctx user password = do
  let loginQuery = [("login", Just user),
                    ("password", Just password)]
      session = anonymousSession ctx
      req = newBzRequest session ["login"] loginQuery
  print $ requestUrl req
  eToken <- try $ sendBzRequest session req
  return $ case eToken of
             Left (BugzillaAPIError 300 _) -> Nothing
             Left e                        -> throw e
             Right token                   -> Just $ LoginSession ctx token

anonymousSession :: BugzillaContext -> BugzillaSession
anonymousSession ctx = AnonymousSession ctx

intAsText :: Int -> T.Text
intAsText = T.pack . show

searchBugs :: BugzillaSession -> SearchExpression -> IO [Bug]
searchBugs session search = do
  let searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] searchQuery
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  return bugs

searchBugsWithLimit :: BugzillaSession -> Int -> Int -> SearchExpression -> IO [Bug]
searchBugsWithLimit session limit offset search = do
  let limitQuery = [("limit", Just $ intAsText limit),
                    ("offset", Just $ intAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (limitQuery ++ searchQuery)
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  return bugs

getBug :: BugzillaSession -> BugId -> IO (Maybe Bug)
getBug session bid = do
  let req = newBzRequest session ["bug", intAsText bid] []
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  case bugs of
    [bug] -> return $ Just bug
    []    -> return Nothing
    _     -> throw $ BugzillaUnexpectedValue
                     "Request for a single bug returned multiple bugs"

getAttachment :: BugzillaSession -> AttachmentId -> IO (Maybe Attachment)
getAttachment session aid = do
  let req = newBzRequest session ["bug", "attachment", intAsText aid] []
  print $ requestUrl req
  (AttachmentList as) <- sendBzRequest session req
  case as of
    [a] -> return $ Just a
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single attachment returned multiple attachments"
  
getAttachments :: BugzillaSession -> BugId -> IO [Attachment]
getAttachments session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "attachment"] []
  print $ requestUrl req
  (AttachmentList as) <- sendBzRequest session req
  return as

getComments :: BugzillaSession -> BugId -> IO [Comment]
getComments session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "comment"] []
  print $ requestUrl req
  (CommentList as) <- sendBzRequest session req
  return as

getHistory :: BugzillaSession -> BugId -> IO History
getHistory session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "history"] []
  print $ requestUrl req
  sendBzRequest session req
