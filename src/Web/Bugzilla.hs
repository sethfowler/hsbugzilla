{-# LANGUAGE OverloadedStrings #-}

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
, BzSession (..)
, loginSession
, anonymousSession
) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.Clock (UTCTime(..))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (mkManagerSettings, newManager, closeManager)

import Web.Bugzilla.Internal

type BzBugId     = Int
type BzUserId    = Int
type BzFlagId    = Int
type BzFlagType  = Int
type BzUserEmail = T.Text

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

-- Remaining features:
--  * Comments
--  * Attachments
--  * History
-- With those implemented, work on the Snap site can start.

loginSession :: BzContext -> BzUserEmail -> T.Text -> IO BzSession
loginSession ctx user password = do
  let loginQuery = [("login", Just user),
                    ("password", Just password)]
      session = anonymousSession ctx
      req = newBzRequest session ["login"] loginQuery
  print $ requestUrl req
  token <- sendBzRequest session req
  return $ LoginSession ctx token

anonymousSession :: BzContext -> BzSession
anonymousSession ctx = AnonymousSession ctx
