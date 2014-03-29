{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla
( BugzillaException
, BzContext
, newBzContext
, closeBzContext
, withBzContext
, BzServer
, BzUserEmail
, User(..)
, Flag(..)
, Bug(..)
, BugField(..)
, standardBugFields
, assignedTo
) where

import Blaze.ByteString.Builder (toByteString)
import Control.Applicative
import Control.Exception (Exception, bracket, throw)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.ByteString as B
import Data.Default (def)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime(..))
import Data.Typeable
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (Manager, Request(..), Response(..), host, path, queryString, secure,
                             httpLbs, mkManagerSettings, newManager, closeManager)
import Network.HTTP.Types.URI (QueryText, encodePathSegments, renderQueryText)

data BugzillaException = BugzillaException String
  deriving (Show, Typeable)
instance Exception BugzillaException

type BzBugId       = Int
type BzUserEmailId = Int
type BzFlagId      = Int
type BzFlagType    = Int
type BzServer      = T.Text
type BzUserEmail   = T.Text
type QueryPart     = (T.Text, Maybe T.Text)

sslRequest :: Request
sslRequest =
  def {
    secure = True,
    port   = 443
  }

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

data BzContext = BzContext
  { bzServer  :: BzServer
  , bzManager :: Manager
  }

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
  { userId       :: !BzUserEmailId
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
  parseJSON _ = mzero
  
data Bug = Bug
  { bugId                  :: !BzBugId
  , bugAlias               :: Maybe T.Text
  , bugAssignedTo          :: Maybe BzUserEmail
  , bugAssignedToDetail    :: Maybe User
  , bugBlocks              :: Maybe [BzBugId]
  , bugCc                  :: Maybe [BzUserEmail]
  , bugCcDetail            :: Maybe [User]
  , bugClassification      :: Maybe T.Text
  , bugComponent           :: Maybe T.Text
  , bugCreationTime        :: Maybe UTCTime
  , bugCreator             :: Maybe BzUserEmail
  , bugCreatorDetail       :: Maybe User
  , bugDependsOn           :: Maybe [BzBugId]
  , bugDupeOf              :: Maybe BzBugId
  , bugFlags               :: Maybe [Flag]
  , bugGroups              :: Maybe [T.Text]
  , bugIsCcAccessible      :: Maybe Bool
  , bugIsConfirmed         :: Maybe Bool
  , bugIsCreatorAccessible :: Maybe Bool
  , bugIsOpen              :: Maybe Bool
  , bugKeywords            :: Maybe [T.Text]
  , bugLastChangeTime      :: Maybe UTCTime
  , bugOpSys               :: Maybe T.Text
  , bugPlatform            :: Maybe T.Text
  , bugPriority            :: Maybe T.Text
  , bugProduct             :: Maybe T.Text
  , bugQaContact           :: Maybe BzUserEmail
  , bugResolution          :: Maybe T.Text
  , bugSeeAlso             :: Maybe [T.Text]
  , bugSeverity            :: Maybe T.Text
  , bugStatus              :: Maybe T.Text
  , bugSummary             :: Maybe T.Text
  , bugTargetMilestone     :: Maybe T.Text
  , bugUrl                 :: Maybe T.Text
  , bugVersion             :: Maybe T.Text
  , bugWhiteboard          :: Maybe T.Text
  , bugCustomFields        :: H.HashMap T.Text T.Text
  } deriving (Eq, Show)

instance FromJSON Bug where
  parseJSON (Object v) =
      Bug <$> v .:  "id"
          <*> v .:? "alias"
          <*> v .:? "assigned_to"
          <*> v .:? "assigned_to_detail"
          <*> v .:? "blocks"
          <*> v .:? "cc"
          <*> v .:? "cc_detail"
          <*> v .:? "classification"
          <*> v .:? "component"
          <*> v .:? "creation_time"
          <*> v .:? "creator"
          <*> v .:? "creator_detail"
          <*> v .:? "depends_on"
          <*> v .:? "dupe_of"
          <*> v .:? "flags"
          <*> v .:? "groups"
          <*> v .:? "is_cc_accessible"
          <*> v .:? "is_confirmed"
          <*> v .:? "is_creator_accessible"
          <*> v .:? "is_open"
          <*> v .:? "keywords"
          <*> v .:? "last_change_time"
          <*> v .:? "op_sys"
          <*> v .:? "platform"
          <*> v .:? "priority"
          <*> v .:? "product"
          <*> v .:? "qa_contact"
          <*> v .:? "resolution"
          <*> v .:? "see_also"
          <*> v .:? "severity"
          <*> v .:? "status"
          <*> v .:? "summary"
          <*> v .:? "target_milestone"
          <*> v .:? "url"
          <*> v .:? "version"
          <*> v .:? "whiteboard"
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
    
data BugField
  = Alias
  | AssignedTo
  | AssignedToDetail
  | Blocks
  | Cc
  | CcDetail
  | Classification
  | Component
  | CreationTime
  | Creator
  | CreatorDetail
  | DependsOn
  | DupeOf
  | Flags
  | Groups
  | Id
  | IsCcAccessible
  | IsConfirmed
  | IsCreatorAccessible
  | IsOpen
  | Keywords
  | LastChangeTime
  | OpSys
  | Platform
  | Priority
  | Product
  | QaContact
  | Resolution
  | SeeAlso
  | Severity
  | Status
  | Summary
  | TargetMilestone
  | Url
  | Version
  | Whiteboard
  | CustomBugField T.Text
    deriving (Eq, Ord, Show)

standardBugFields :: S.Set BugField
standardBugFields = S.fromList
  [Alias, AssignedTo, AssignedToDetail, Blocks, Cc, CcDetail, Classification,
  Component, CreationTime, Creator, CreatorDetail, DependsOn, DupeOf, Flags,
  Groups, Id, IsCcAccessible, IsConfirmed, IsCreatorAccessible, IsOpen,
  Keywords, LastChangeTime, OpSys, Platform, Priority, Product, QaContact,
  Resolution, SeeAlso, Severity, Status, Summary, TargetMilestone, Url, Version, Whiteboard]

bugFieldName :: BugField -> T.Text
bugFieldName Alias                 = "alias"
bugFieldName AssignedTo            = "assigned_to"
bugFieldName AssignedToDetail      = "assigned_to_detail"
bugFieldName Blocks                = "blocks"
bugFieldName Cc                    = "cc"
bugFieldName CcDetail              = "cc_detail"
bugFieldName Classification        = "classification"
bugFieldName Component             = "component"
bugFieldName CreationTime          = "creation_time"
bugFieldName Creator               = "creator"
bugFieldName CreatorDetail         = "creator_detail"
bugFieldName DependsOn             = "depends_on"
bugFieldName DupeOf                = "dupe_of"
bugFieldName Flags                 = "flags"
bugFieldName Groups                = "groups"
bugFieldName Id                    = "id"
bugFieldName IsCcAccessible        = "is_cc_accessible"
bugFieldName IsConfirmed           = "is_confirmed"
bugFieldName IsCreatorAccessible   = "is_creator_accessible"
bugFieldName IsOpen                = "is_open"
bugFieldName Keywords              = "keywords"
bugFieldName LastChangeTime        = "last_change_time"
bugFieldName OpSys                 = "op_sys"
bugFieldName Platform              = "platform"
bugFieldName Priority              = "priority"
bugFieldName Product               = "product"
bugFieldName QaContact             = "qa_contact"
bugFieldName Resolution            = "resolution"
bugFieldName SeeAlso               = "see_also"
bugFieldName Severity              = "severity"
bugFieldName Status                = "status"
bugFieldName Summary               = "summary"
bugFieldName TargetMilestone       = "target_milestone"
bugFieldName Url                   = "url"
bugFieldName Version               = "version"
bugFieldName Whiteboard            = "whiteboard"
bugFieldName (CustomBugField name) = name

includeFields :: S.Set BugField -> QueryPart
includeFields fields = ("include_fields", Just $ names fields)
  where names = T.intercalate "," . map bugFieldName . S.toList

queryFields :: [(BugField, T.Text)] -> [QueryPart]
queryFields = map toQueryPart
  where toQueryPart (field, value) = (bugFieldName field, Just value)

bzRequest :: BzContext -> [T.Text] -> QueryText -> Request
bzRequest ctx methodParts query =
  sslRequest {
    host        = TE.encodeUtf8 $ bzServer ctx,
    path        = toByteString $ encodePathSegments $ "rest" : methodParts,
    queryString = toByteString $ renderQueryText True query
  }

data SearchResult = SearchResult [Bug]
                    deriving (Eq, Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "bugs"
  parseJSON _          = mzero
  
bzSearchBugsRequest :: BzContext -> S.Set BugField -> [(BugField, T.Text)] -> Request
bzSearchBugsRequest ctx include query = bzRequest ctx ["bug"] $ is : qs
  where
    is = includeFields $ Id `S.insert` include
    qs = queryFields query

assignedTo :: BzContext -> S.Set BugField -> BzUserEmail -> IO [Bug]
assignedTo ctx fields user = runResourceT $ do
  let req = bzSearchBugsRequest ctx fields [(AssignedTo, user)]
  liftIO $ print $ requestUrl req
  response <- liftIO $ httpLbs req (bzManager ctx)
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg                  -> throw $ BugzillaException $ "JSON parse error: " ++ msg
    Right (SearchResult bugs) -> return bugs
