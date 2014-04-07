{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | This package is designed to provide an easy-to-use, typesafe
--   interface to querying Bugzilla from Haskell.
--
--   A very simple program using this package might look like this:
--
-- > withBugzillaContext "bugzilla.example.org" $ \ctx -> do
-- >   let session = anonymousSession ctx
-- >       user = "me@example.org"
-- >       query = AssignedToField .==. user .&&.
-- >               FlagRequesteeField .==. user .&&.
-- >               (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
-- >   bugs <- searchBugs session query
-- >   mapM_ (putStrLn . show . bugSummary) bugs
--
--   There's a somewhat more in-depth demo program included with the
--   source code to this package.
module Web.Bugzilla
( -- * Connecting to Bugzilla
  newBugzillaContext
, closeBugzillaContext
, withBugzillaContext
, loginSession
, anonymousSession

, BugzillaServer
, BugzillaContext
, BugzillaSession (..)
, BugzillaToken

  -- * Querying Bugzilla
, searchBugs
, searchBugsWithLimit
, getBug
, getAttachment
, getAttachments
, getComments
, getHistory
, searchUsers
, getUser
, getUserById


, BugId
, AttachmentId
, CommentId
, UserId
, EventId
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
, HistoryEvent (..)
, Change (..)
, Modification (..)
, fieldName

, BugzillaException (..)
) where

import Control.Exception (bracket, throw, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (mkManagerSettings, newManager, closeManager)

import Web.Bugzilla.Internal.Network
import Web.Bugzilla.Internal.Search
import Web.Bugzilla.Internal.Types

-- | Creates a new 'BugzillaContext', suitable for connecting to the
--   provided server. You should try to reuse 'BugzillaContext's
--   whenever possible, because creating them is expensive.
newBugzillaContext :: BugzillaServer -> IO BugzillaContext
newBugzillaContext server = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- liftIO $ newManager settings
  return $ BugzillaContext server manager

-- | Closes the provided 'BugzillaContext'. Using it afterwards is an error.
closeBugzillaContext :: BugzillaContext -> IO ()
closeBugzillaContext = closeManager . bzManager

-- | Creates a 'BugzillaContext' and ensures that it will be closed
--   automatically, even if an exception is thrown.
withBugzillaContext :: BugzillaServer -> (BugzillaContext -> IO a) -> IO a
withBugzillaContext server f = bracket (newBugzillaContext server) closeBugzillaContext f

-- | Attempts to create a logged-in 'BugzillaSession' using the
--   provided username and password. Returns 'Nothing' if login
--   fails.
loginSession :: BugzillaContext -> UserEmail -> T.Text -> IO (Maybe BugzillaSession)
loginSession ctx user password = do
  let loginQuery = [("login", Just user),
                    ("password", Just password)]
      session = anonymousSession ctx
      req = newBzRequest session ["login"] loginQuery
  eToken <- try $ sendBzRequest session req
  return $ case eToken of
             Left (BugzillaAPIError 300 _) -> Nothing
             Left e                        -> throw e
             Right token                   -> Just $ LoginSession ctx token

-- | Creates an anonymous 'BugzillaSession'. Note that some content
--   will be hidden by Bugzilla when you make queries in this state.
anonymousSession :: BugzillaContext -> BugzillaSession
anonymousSession ctx = AnonymousSession ctx

intAsText :: Int -> T.Text
intAsText = T.pack . show

-- | Search Bugzilla and returns a list of 'Bug's. The 'SearchExpression'
-- can be constructed conveniently using the operators in "Web.Bugzilla.Search".
searchBugs :: BugzillaSession -> SearchExpression -> IO [Bug]
searchBugs session search = do
  let searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] searchQuery
  (BugList bugs) <- sendBzRequest session req
  return bugs

-- | Search Bugzilla and returns a limited number of results. You can
--   call this repeatedly and use 'offset' to retrieve the results of
--   a large query incrementally. Note that most Bugzillas won't
--   return all of the results for a very large query by default, but
--   you can request this by calling 'searchBugsWithLimit' with 0 for
--   the limit.
searchBugsWithLimit :: BugzillaSession
                    -> Int  -- ^ The maximum number of results to return.
                    -> Int  -- ^ The offset from the first result to start from.
                    -> SearchExpression
                    -> IO [Bug]
searchBugsWithLimit session limit offset search = do
  let limitQuery = [("limit", Just $ intAsText limit),
                    ("offset", Just $ intAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (limitQuery ++ searchQuery)
  (BugList bugs) <- sendBzRequest session req
  return bugs

-- | Retrieve a bug by bug number.
getBug :: BugzillaSession -> BugId -> IO (Maybe Bug)
getBug session bid = do
  let req = newBzRequest session ["bug", intAsText bid] []
  (BugList bugs) <- sendBzRequest session req
  case bugs of
    [bug] -> return $ Just bug
    []    -> return Nothing
    _     -> throw $ BugzillaUnexpectedValue
                     "Request for a single bug returned multiple bugs"

-- | Retrieve a bug by attachment number.
getAttachment :: BugzillaSession -> AttachmentId -> IO (Maybe Attachment)
getAttachment session aid = do
  let req = newBzRequest session ["bug", "attachment", intAsText aid] []
  (AttachmentList as) <- sendBzRequest session req
  case as of
    [a] -> return $ Just a
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single attachment returned multiple attachments"
  
-- | Get all attachments for a bug.
getAttachments :: BugzillaSession -> BugId -> IO [Attachment]
getAttachments session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "attachment"] []
  (AttachmentList as) <- sendBzRequest session req
  return as

-- | Get all comments for a bug.
getComments :: BugzillaSession -> BugId -> IO [Comment]
getComments session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "comment"] []
  (CommentList as) <- sendBzRequest session req
  return as

-- | Get the history for a bug.
getHistory :: BugzillaSession -> BugId -> IO History
getHistory session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "history"] []
  sendBzRequest session req

-- | Search user names and emails using a substring search.
searchUsers :: BugzillaSession -> T.Text -> IO [User]
searchUsers session text = do
  let req = newBzRequest session ["user"] [("match", Just text)]
  (UserList users) <- sendBzRequest session req
  return users

-- | Get a user by email.
getUser :: BugzillaSession -> UserEmail -> IO (Maybe User)
getUser session user = do
  let req = newBzRequest session ["user", user] []
  (UserList users) <- sendBzRequest session req
  case users of
    [u] -> return $ Just u
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single user returned multiple users"

-- | Get a user by user ID.
getUserById :: BugzillaSession -> UserId -> IO (Maybe User)
getUserById session uid = do
  let req = newBzRequest session ["user", intAsText uid] []
  (UserList users) <- sendBzRequest session req
  case users of
    [u] -> return $ Just u
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single user returned multiple users"
