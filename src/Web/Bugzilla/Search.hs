{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Search
( BugField (..)
, standardBugFields
, SearchField (..)
, SearchTerm (..)
, searchBugs
) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.ISO8601 (formatISO8601)
import Network.HTTP.Conduit (Request(..), Response(..), httpLbs)

import Web.Bugzilla
import Web.Bugzilla.Internal

data BugField
  = BugFieldAlias
  | BugFieldAssignedTo
  | BugFieldAssignedToDetail
  | BugFieldBlocks
  | BugFieldCc
  | BugFieldCcDetail
  | BugFieldClassification
  | BugFieldComponent
  | BugFieldCreationTime
  | BugFieldCreator
  | BugFieldCreatorDetail
  | BugFieldDependsOn
  | BugFieldDupeOf
  | BugFieldFlags
  | BugFieldGroups
  | BugFieldId
  | BugFieldIsCcAccessible
  | BugFieldIsConfirmed
  | BugFieldIsCreatorAccessible
  | BugFieldIsOpen
  | BugFieldKeywords
  | BugFieldLastChangeTime
  | BugFieldOpSys
  | BugFieldPlatform
  | BugFieldPriority
  | BugFieldProduct
  | BugFieldQaContact
  | BugFieldResolution
  | BugFieldSeeAlso
  | BugFieldSeverity
  | BugFieldStatus
  | BugFieldSummary
  | BugFieldTargetMilestone
  | BugFieldUrl
  | BugFieldVersion
  | BugFieldWhiteboard
  | CustomBugField T.Text
    deriving (Eq, Ord, Show)

standardBugFields :: S.Set BugField
standardBugFields = S.fromList
  [BugFieldAlias, BugFieldAssignedTo, BugFieldAssignedToDetail, BugFieldBlocks, BugFieldCc, BugFieldCcDetail,
   BugFieldClassification, BugFieldComponent, BugFieldCreationTime, BugFieldCreator, BugFieldCreatorDetail,
   BugFieldDependsOn, BugFieldDupeOf, BugFieldFlags, BugFieldGroups, BugFieldId, BugFieldIsCcAccessible,
   BugFieldIsConfirmed, BugFieldIsCreatorAccessible, BugFieldIsOpen, BugFieldKeywords, BugFieldLastChangeTime,
   BugFieldOpSys, BugFieldPlatform, BugFieldPriority, BugFieldProduct, BugFieldQaContact, BugFieldResolution,
   BugFieldSeeAlso, BugFieldSeverity, BugFieldStatus, BugFieldSummary, BugFieldTargetMilestone, BugFieldUrl,
   BugFieldVersion, BugFieldWhiteboard]

bugFieldName :: BugField -> T.Text
bugFieldName BugFieldAlias                 = "alias"
bugFieldName BugFieldAssignedTo            = "assigned_to"
bugFieldName BugFieldAssignedToDetail      = "assigned_to_detail"
bugFieldName BugFieldBlocks                = "blocks"
bugFieldName BugFieldCc                    = "cc"
bugFieldName BugFieldCcDetail              = "cc_detail"
bugFieldName BugFieldClassification        = "classification"
bugFieldName BugFieldComponent             = "component"
bugFieldName BugFieldCreationTime          = "creation_time"
bugFieldName BugFieldCreator               = "creator"
bugFieldName BugFieldCreatorDetail         = "creator_detail"
bugFieldName BugFieldDependsOn             = "depends_on"
bugFieldName BugFieldDupeOf                = "dupe_of"
bugFieldName BugFieldFlags                 = "flags"
bugFieldName BugFieldGroups                = "groups"
bugFieldName BugFieldId                    = "id"
bugFieldName BugFieldIsCcAccessible        = "is_cc_accessible"
bugFieldName BugFieldIsConfirmed           = "is_confirmed"
bugFieldName BugFieldIsCreatorAccessible   = "is_creator_accessible"
bugFieldName BugFieldIsOpen                = "is_open"
bugFieldName BugFieldKeywords              = "keywords"
bugFieldName BugFieldLastChangeTime        = "last_change_time"
bugFieldName BugFieldOpSys                 = "op_sys"
bugFieldName BugFieldPlatform              = "platform"
bugFieldName BugFieldPriority              = "priority"
bugFieldName BugFieldProduct               = "product"
bugFieldName BugFieldQaContact             = "qa_contact"
bugFieldName BugFieldResolution            = "resolution"
bugFieldName BugFieldSeeAlso               = "see_also"
bugFieldName BugFieldSeverity              = "severity"
bugFieldName BugFieldStatus                = "status"
bugFieldName BugFieldSummary               = "summary"
bugFieldName BugFieldTargetMilestone       = "target_milestone"
bugFieldName BugFieldUrl                   = "url"
bugFieldName BugFieldVersion               = "version"
bugFieldName BugFieldWhiteboard            = "whiteboard"
bugFieldName (CustomBugField name) = name


data SearchField
  = Alias                  -- Alias
  | AssignedTo             -- Assignee
  | AttachmentsSubmitter   -- Attachment creator
  | AttachmentsData        -- Attachment data
  | AttachmentsDescription -- Attachment description
  | AttachmentsFilename    -- Attachment filename
  | AttachmentsIsObsolete  -- Attachment is obsolete
  | AttachmentsIsPatch     -- Attachment is patch
  | AttachmentsIsPrivate   -- Attachment is private
  | AttachmentsMimetype    -- Attachment mime type
  | Blocked                -- Blocks
  | BugId                  -- Bug ID
  | Cc                     -- CC
  | CcListAccessible       -- CC list accessible
  | Classification         -- Classification
  | Longdesc               -- Comment
  | LongdescsIsPrivate     -- Comment is private
  | CommentTag             -- Comment Tag
  | Commenter              -- Commenter
  | Component              -- Component
  | Content                -- Content
  | CreationTs             -- Creation date
  | DaysElapsed            -- Days since bug changed
  | Dependson              -- Depends on
  | EverConfirmed          -- Ever confirmed
  | RequesteesLoginName    -- Flag Requestee
  | SettersLoginName       -- Flag Setter
  | FlagTypesName          -- Flags
  | BugGroup               -- Group
  | Keywords               -- Keywords
  | DeltaTs                -- Changed
  | LongdescsCount         -- Number of Comments
  | OpSys                  -- OS
  | RepPlatform            -- Hardware
  | Priority               -- Priority
  | Product                -- Product
  | QaContact              -- QA Contact
  | Reporter               -- Reporter
  | ReporterAccessible     -- Reporter accessible
  | Resolution             -- Resolution
  | RestrictComments       -- Restrict Comments
  | SeeAlso                -- See Also
  | BugSeverity            -- Severity
  | BugStatus              -- Status
  | StatusWhiteboard       -- Whiteboard
  | ShortDesc              -- Summary
  | Tag                    -- Tags
  | TargetMilestone        -- Target Milestone
  | OwnerIdleTime          -- Time Since Assignee Touched
  | BugFileLoc             -- URL
  | Version                -- Version
  | Votes                  -- Votes
    deriving (Bounded, Enum, Eq, Ord, Show)

searchFieldName :: SearchField -> T.Text
searchFieldName Alias = "alias"
searchFieldName AssignedTo = "assigned_to"
searchFieldName AttachmentsSubmitter = "attachments.submitter"
searchFieldName AttachmentsData = "attach_data.thedata"
searchFieldName AttachmentsDescription = "attachments.description"
searchFieldName AttachmentsFilename = "attachments.filename"
searchFieldName AttachmentsIsObsolete = "attachments.isobsolete"
searchFieldName AttachmentsIsPatch = "attachments.ispatch"
searchFieldName AttachmentsIsPrivate = "attachments.isprivate"
searchFieldName AttachmentsMimetype = "attachments.mimetype"
searchFieldName Blocked = "blocked"
searchFieldName BugId = "bug_id"
searchFieldName Cc = "cc"
searchFieldName CcListAccessible = "cclist_accessible"
searchFieldName Classification = "classification"
searchFieldName Longdesc = "longdesc"
searchFieldName LongdescsIsPrivate = "longdescs.isprivate"
searchFieldName CommentTag = "comment_tag"
searchFieldName Commenter = "commenter"
searchFieldName Component = "component"
searchFieldName Content = "content"
searchFieldName CreationTs = "creation_ts"
searchFieldName DaysElapsed = "days_elapsed"
searchFieldName Dependson = "dependson"
searchFieldName EverConfirmed = "everconfirmed"
searchFieldName RequesteesLoginName = "requestees.login_name"
searchFieldName SettersLoginName = "setters.login_name"
searchFieldName FlagTypesName = "flagtypes.name"
searchFieldName BugGroup = "bug_group"
searchFieldName Keywords = "keywords"
searchFieldName DeltaTs = "delta_ts"
searchFieldName LongdescsCount = "longdescs.count"
searchFieldName OpSys = "op_sys"
searchFieldName RepPlatform = "rep_platform"
searchFieldName Priority = "priority"
searchFieldName Product = "product"
searchFieldName QaContact = "qa_contact"
searchFieldName Reporter = "reporter"
searchFieldName ReporterAccessible = "reporter_accessible"
searchFieldName Resolution = "resolution"
searchFieldName RestrictComments = "restrict_comments"
searchFieldName SeeAlso = "see_also"
searchFieldName BugSeverity = "bug_severity"
searchFieldName BugStatus = "bug_status"
searchFieldName StatusWhiteboard = "status_whiteboard"
searchFieldName ShortDesc = "short_desc"
searchFieldName Tag = "tag"
searchFieldName TargetMilestone = "target_milestone"
searchFieldName OwnerIdleTime = "owner_idle_time"
searchFieldName BugFileLoc = "bug_file_loc"
searchFieldName Version = "version"
searchFieldName Votes = "votes"

data SearchTerm
  = Equals SearchField T.Text
  | NotEquals SearchField T.Text
  | StringEqualsAny SearchField T.Text
  | StringsContains SearchField T.Text
  | StringContainsMatchingCase SearchField T.Text
  | StringDoesNotContain SearchField T.Text
  | StringContainsAny SearchField T.Text
  | StringContainsAll SearchField T.Text
  | StringContainsNone SearchField T.Text
  | RegexpMatches SearchField T.Text
  | RegexpNotMatches SearchField T.Text
  | LessThan SearchField T.Text
  | LessThanOrEqual SearchField T.Text
  | GreaterThan SearchField T.Text
  | GreaterThanOrEqual SearchField T.Text
  | WordsContainsAny SearchField T.Text
  | WordsContainsAll SearchField T.Text
  | WordsContainsNone SearchField T.Text
  | ChangedBefore SearchField UTCTime
  | ChangedAfter SearchField UTCTime
  | ChangedFrom SearchField T.Text
  | ChangedTo SearchField T.Text
  | ChangedBy SearchField BzUserEmail
  | ContentMatches T.Text
  | ContentNotMatches T.Text
  | IsEmpty SearchField
  | IsNotEmpty SearchField
    deriving (Eq, Show)

-- TODO: Support expressions.
searchQuery :: SearchField -> T.Text -> T.Text -> [QueryPart]
searchQuery f o v = [("f1", Just $ searchFieldName f),
                     ("o1", Just o),
                     ("v1", Just v)]

evalSearch :: SearchTerm -> [QueryPart]
evalSearch (Equals field val)                     = searchQuery field "equals" val
evalSearch (NotEquals field val)                  = searchQuery field "notequals" val
evalSearch (StringEqualsAny field val)            = searchQuery field "anyexact" val
evalSearch (StringsContains field val)            = searchQuery field "substring" val
evalSearch (StringContainsMatchingCase field val) = searchQuery field "casesubstring" val
evalSearch (StringDoesNotContain field val)       = searchQuery field "notsubstring" val
evalSearch (StringContainsAny field val)          = searchQuery field "anywordssubstr" val
evalSearch (StringContainsAll field val)          = searchQuery field "allwordssubstr" val
evalSearch (StringContainsNone field val)         = searchQuery field "nowordssubstr" val
evalSearch (RegexpMatches field val)              = searchQuery field "regexp" val
evalSearch (RegexpNotMatches field val)           = searchQuery field "notregexp" val
evalSearch (LessThan field val)                   = searchQuery field "lessthan" val
evalSearch (LessThanOrEqual field val)            = searchQuery field "lessthaneq" val
evalSearch (GreaterThan field val)                = searchQuery field "greaterthan" val
evalSearch (GreaterThanOrEqual field val)         = searchQuery field "greaterthaneq" val
evalSearch (WordsContainsAny field val)           = searchQuery field "anywords" val
evalSearch (WordsContainsAll field val)           = searchQuery field "allwords" val
evalSearch (WordsContainsNone field val)          = searchQuery field "nowords" val
evalSearch (ChangedBefore field date)             = searchQuery field "changedbefore" (T.pack $ formatISO8601 date)
evalSearch (ChangedAfter field date)              = searchQuery field "changedafter" (T.pack $ formatISO8601 date)
evalSearch (ChangedFrom field val)                = searchQuery field "changedfrom" val
evalSearch (ChangedTo field val)                  = searchQuery field "changedto" val
evalSearch (ChangedBy field val)                  = searchQuery field "changedby" val
evalSearch (ContentMatches val)                   = searchQuery Content "matches" val
evalSearch (ContentNotMatches val)                = searchQuery Content "notmatches" val
evalSearch (IsEmpty field)                        = searchQuery field "isempty" ""
evalSearch (IsNotEmpty field)                     = searchQuery field "isnotempty" ""

includeFields :: S.Set BugField -> QueryPart
includeFields fields = ("include_fields", Just $ names fields)
  where names = T.intercalate "," . map bugFieldName . S.toList

data SearchResult = SearchResult [Bug]
                    deriving (Eq, Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "bugs"
  parseJSON _          = mzero
  
bzSearchBugsRequest :: BzContext -> S.Set BugField -> SearchTerm -> Request
bzSearchBugsRequest ctx include search = bzRequest ctx ["bug"] $ is : qs
  where
    is = includeFields $ BugFieldId `S.insert` include
    qs = evalSearch search

searchBugs :: BzContext -> S.Set BugField -> SearchTerm -> IO [Bug]
searchBugs ctx fields search = runResourceT $ do
  let req = bzSearchBugsRequest ctx fields search
  liftIO $ print $ requestUrl req
  response <- liftIO $ httpLbs req (bzManager ctx)
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg                  -> throw $ BugzillaException $ "JSON parse error: " ++ msg
    Right (SearchResult bugs) -> return bugs