{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Web.Bugzilla
( BzException (..)
, BzContext
, newBzContext
, closeBzContext
, withBzContext
, BzServer
, BzUserEmail
, User (..)
, Flag (..)
, Bug (..)
, BugList (..)
, Attachment (..)
, Comment (..)
, BzSession (..)
, loginSession
, anonymousSession
, getBug
, getAttachment
, getAttachments
, getComments
) where

import Control.Applicative
import Control.Exception (bracket, throw, try)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import Data.Time.Clock (UTCTime(..))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (mkManagerSettings, newManager, closeManager)

import Web.Bugzilla.Internal

type BzBugId        = Int
type BzAttachmentId = Int
type BzCommentId    = Int
type BzUserId       = Int
type BzFlagId       = Int
type BzFlagType     = Int
type BzUserEmail    = T.Text

newBzContext :: BzServer -> IO BzContext
newBzContext server = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- liftIO $ newManager settings
  return $ BzContext server manager

closeBzContext :: BzContext -> IO ()
closeBzContext = closeManager . bzManager

withBzContext :: BzServer -> (BzContext -> IO a) -> IO a
withBzContext server f = bracket (newBzContext server) closeBzContext f

data User = User
  { userId       :: !BzUserId
  , userEmail    :: T.Text
  , userName     :: T.Text
  , userRealName :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "id"
         <*> v .: "email"
         <*> v .: "name"
         <*> v .: "real_name"
  parseJSON _ = mzero

data Flag = Flag
  { flagId               :: !BzFlagId
  , flagTypeId           :: !BzFlagType
  , flagName             :: T.Text
  , flagSetter           :: BzUserEmail
  , flagStatus           :: T.Text
  , flagCreationDate     :: UTCTime
  , flagModificationDate :: UTCTime
  , flagRequestee        :: Maybe BzUserEmail
  } deriving (Eq, Ord, Show)
             
instance FromJSON Flag where
  parseJSON (Object v) =
    Flag <$> v .: "id"
         <*> v .: "type_id"
         <*> v .: "name"
         <*> v .: "setter"
         <*> v .: "status"
         <*> v .: "creation_date"
         <*> v .: "modification_date"
         <*> v .:? "requestee"
  parseJSON _ = mzero
  
data Bug = Bug
  { bugId                  :: !BzBugId
  , bugAlias               :: Maybe T.Text
  , bugAssignedTo          :: BzUserEmail
  , bugAssignedToDetail    :: User
  , bugBlocks              :: [BzBugId]
  , bugCc                  :: [BzUserEmail]
  , bugCcDetail            :: [User]
  , bugClassification      :: T.Text
  , bugComponent           :: T.Text
  , bugCreationTime        :: UTCTime
  , bugCreator             :: BzUserEmail
  , bugCreatorDetail       :: User
  , bugDependsOn           :: [BzBugId]
  , bugDupeOf              :: Maybe BzBugId
  , bugFlags               :: [Flag]
  , bugGroups              :: [T.Text]
  , bugIsCcAccessible      :: Bool
  , bugIsConfirmed         :: Bool
  , bugIsCreatorAccessible :: Bool
  , bugIsOpen              :: Bool
  , bugKeywords            :: [T.Text]
  , bugLastChangeTime      :: UTCTime
  , bugOpSys               :: T.Text
  , bugPlatform            :: T.Text
  , bugPriority            :: T.Text
  , bugProduct             :: T.Text
  , bugQaContact           :: BzUserEmail
  , bugResolution          :: T.Text
  , bugSeeAlso             :: [T.Text]
  , bugSeverity            :: T.Text
  , bugStatus              :: T.Text
  , bugSummary             :: T.Text
  , bugTargetMilestone     :: T.Text
  , bugUrl                 :: T.Text
  , bugVersion             :: T.Text
  , bugWhiteboard          :: T.Text
  , bugCustomFields        :: H.HashMap T.Text T.Text
  } deriving (Eq, Show)

instance FromJSON Bug where
  parseJSON (Object v) =
      Bug <$> v .:  "id"
          <*> v .:? "alias"
          <*> v .: "assigned_to"
          <*> v .: "assigned_to_detail"
          <*> v .: "blocks"
          <*> v .: "cc"
          <*> v .: "cc_detail"
          <*> v .: "classification"
          <*> v .: "component"
          <*> v .: "creation_time"
          <*> v .: "creator"
          <*> v .: "creator_detail"
          <*> v .: "depends_on"
          <*> v .:? "dupe_of"
          <*> v .: "flags"
          <*> v .: "groups"
          <*> v .: "is_cc_accessible"
          <*> v .: "is_confirmed"
          <*> v .: "is_creator_accessible"
          <*> v .: "is_open"
          <*> v .: "keywords"
          <*> v .: "last_change_time"
          <*> v .: "op_sys"
          <*> v .: "platform"
          <*> v .: "priority"
          <*> v .: "product"
          <*> v .: "qa_contact"
          <*> v .: "resolution"
          <*> v .: "see_also"
          <*> v .: "severity"
          <*> v .: "status"
          <*> v .: "summary"
          <*> v .: "target_milestone"
          <*> v .: "url"
          <*> v .: "version"
          <*> v .: "whiteboard"
          <*> pure (customFields v)
  parseJSON _ = mzero

customFields :: Object -> H.HashMap T.Text T.Text
customFields = H.map stringifyCustomFields
             . H.filterWithKey filterCustomFields
  where
    stringifyCustomFields :: Value -> T.Text
    stringifyCustomFields (String t) = t
    stringifyCustomFields v          = T.concat
                                     . TL.toChunks
                                     . TLB.toLazyText
                                     . encodeToTextBuilder
                                     . toJSON
                                     $ v

    filterCustomFields k _ = "cf_" `T.isPrefixOf` k

data BugList = BugList [Bug]
               deriving (Eq, Show)

instance FromJSON BugList where
  parseJSON (Object v) = BugList <$> v .: "bugs"
  parseJSON _          = mzero

data Attachment = Attachment
  { attachmentId             :: !BzAttachmentId
  , attachmentBugId          :: !BzBugId
  , attachmentFileName       :: T.Text
  , attachmentSummary        :: T.Text
  , attachmentCreator        :: BzUserEmail
  , attachmentIsPrivate      :: Bool
  , attachmentIsObsolete     :: Bool
  , attachmentIsPatch        :: Bool
  , attachmentFlags          :: [Flag]
  , attachmentCreationTime   :: UTCTime
  , attachmentLastChangeTime :: UTCTime
  , attachmentContentType    :: T.Text
  , attachmentSize           :: !Int
  , attachmentData           :: T.Text
  } deriving (Eq, Show)

instance FromJSON Attachment where
  parseJSON (Object v) = do
    Attachment <$> v .: "id"
               <*> v .: "bug_id"
               <*> v .: "file_name"
               <*> v .: "summary"
               <*> v .: "creator"
               <*> (fromNumericBool <$> v .: "is_private")
               <*> (fromNumericBool <$> v .: "is_obsolete")
               <*> (fromNumericBool <$> v .: "is_patch")
               <*> v .: "flags"
               <*> v .: "creation_time"
               <*> v .: "last_change_time"
               <*> v .: "content_type"
               <*> v .: "size"
               <*> v .: "data"
  parseJSON _ = mzero

fromNumericBool :: Int -> Bool
fromNumericBool 0 = False
fromNumericBool _ = True

data AttachmentList = AttachmentList [Attachment]
                      deriving (Eq, Show)

instance FromJSON AttachmentList where
  parseJSON (Object v) = do
    attachmentsVal <- v .: "attachments"
    bugsVal <- v .: "bugs"
    case (attachmentsVal, bugsVal) of
      (Object (H.toList -> [(_, as)]), _) -> AttachmentList . (:[]) <$> parseJSON as
      (_, Object (H.toList -> [(_, as)])) -> AttachmentList <$> parseJSON as
      _                                   -> mzero
  parseJSON _ = mzero

data Comment = Comment
  { commentId           :: !BzCommentId
  , commentBugId        :: !BzBugId
  , commentAttachmentId :: Maybe BzAttachmentId
  , commentCount        :: !Int
  , commentText         :: T.Text
  , commentCreator      :: BzUserEmail
  , commentCreationTime :: UTCTime
  , commentIsPrivate    :: Bool
  } deriving (Eq, Show)

instance FromJSON Comment where
  parseJSON (Object v) = do
    Comment <$> v .: "id"
            <*> v .: "bug_id"
            <*> v .: "attachment_id"
            <*> v .: "count"
            <*> v .: "text"
            <*> v .: "creator"
            <*> v .: "creation_time"
            <*> v .: "is_private"
  parseJSON _ = mzero

data CommentList = CommentList [Comment]
                   deriving (Eq, Show)

instance FromJSON CommentList where
  parseJSON (Object v) = do
    bugsVal <- v .: "bugs"
    case bugsVal of
      Object (H.toList -> [(_, cs)]) ->
        do comments <- withObject "comments" (.: "comments") cs
           withArray "comment list" (\a -> CommentList <$> parseJSON (addCount a)) comments
      _ -> mzero
  parseJSON _ = mzero

-- Note that we make the (possibly unwise) assumption that Bugzilla
-- returns the comments in order. If it turns out that's not true, we
-- can always sort by their 'id' to ensure correct results.
addCount :: V.Vector Value -> Value 
addCount vs = Array $ V.zipWith addCount' (V.enumFromN 0 $ V.length vs) vs
 where
   addCount' :: Int -> Value -> Value
   addCount' c (Object v) = Object $ H.insert "count" (Number $ fromIntegral c) v
   addCount' _ v          = v

loginSession :: BzContext -> BzUserEmail -> T.Text -> IO (Maybe BzSession)
loginSession ctx user password = do
  let loginQuery = [("login", Just user),
                    ("password", Just password)]
      session = anonymousSession ctx
      req = newBzRequest session ["login"] loginQuery
  print $ requestUrl req
  eToken <- try $ sendBzRequest session req
  return $ case eToken of
             Left (BzAPIError 300 _) -> Nothing
             Left e                  -> throw e
             Right token             -> Just $ LoginSession ctx token

anonymousSession :: BzContext -> BzSession
anonymousSession ctx = AnonymousSession ctx

idAsText :: Int -> T.Text
idAsText = T.pack . show

getBug :: BzSession -> BzBugId -> IO (Maybe Bug)
getBug session bid = do
  let req = newBzRequest session ["bug", idAsText bid] []
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  case bugs of
    [bug] -> return $ Just bug
    []    -> return Nothing
    _     -> throw $ BzUnexpectedValue "Request for a single bug returned multiple bugs"

getAttachment :: BzSession -> BzAttachmentId -> IO (Maybe Attachment)
getAttachment session aid = do
  let req = newBzRequest session ["bug", "attachment", idAsText aid] []
  print $ requestUrl req
  (AttachmentList as) <- sendBzRequest session req
  case as of
    [a] -> return $ Just a
    []  -> return Nothing
    _   -> throw $ BzUnexpectedValue "Request for a single attachment returned multiple attachments"
  
getAttachments :: BzSession -> BzBugId -> IO [Attachment]
getAttachments session bid = do
  let req = newBzRequest session ["bug", idAsText bid, "attachment"] []
  print $ requestUrl req
  (AttachmentList as) <- sendBzRequest session req
  return as

getComments :: BzSession -> BzBugId -> IO [Comment]
getComments session bid = do
  let req = newBzRequest session ["bug", idAsText bid, "comment"] []
  print $ requestUrl req
  (CommentList as) <- sendBzRequest session req
  return as

-- Remaining features:
--  * History
-- With those implemented, work on the Snap site can start.

