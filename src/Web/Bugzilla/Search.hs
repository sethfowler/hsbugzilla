{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Search
( BugField (..)
, standardBugFields
, SearchField (..)
, SearchTerm (..)
, SearchExprItem
, SearchExpr (..)
, (.&&.)
, (.||.)
, (.==.)
, (./=.)
, (.<.)
, (.<=.)
, (.>.)
, (.>=.)
, (.=~.)
, (./=~.)
, contains
, notE
, searchBugs
) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad (MonadPlus, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.List
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
  | BugFieldOperatingSystem
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
   BugFieldOperatingSystem, BugFieldPlatform, BugFieldPriority, BugFieldProduct, BugFieldQaContact, BugFieldResolution,
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
bugFieldName BugFieldOperatingSystem                 = "op_sys"
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

class FieldValue a where fvAsText :: a -> T.Text
instance FieldValue T.Text where fvAsText = id
instance FieldValue Int where fvAsText = T.pack . show
instance FieldValue UTCTime where fvAsText = T.pack . formatISO8601
instance FieldValue Bool where
  fvAsText True  = "true"
  fvAsText False = "false"

data SearchField a where
  Alias                    :: SearchField T.Text      -- Alias
  AssignedTo               :: SearchField BzUserEmail -- Assignee
  AttachmentCreator        :: SearchField BzUserEmail -- Attachment creator
  AttachmentData           :: SearchField T.Text      -- Attachment data
  AttachmentDescription    :: SearchField T.Text      -- Attachment description
  AttachmentFilename       :: SearchField T.Text      -- Attachment filename
  AttachmentIsObsolete     :: SearchField Bool        -- Attachment is obsolete
  AttachmentIsPatch        :: SearchField Bool        -- Attachment is patch
  AttachmentIsPrivate      :: SearchField Bool        -- Attachment is private
  AttachmentMimetype       :: SearchField T.Text      -- Attachment mime type
  Blocks                   :: SearchField Int         -- Blocks
  BugId                    :: SearchField Int         -- Bug ID
  Cc                       :: SearchField BzUserEmail -- CC
  CcListAccessible         :: SearchField Bool        -- CC list accessible
  Classification           :: SearchField T.Text      -- Classification
  Comment                  :: SearchField T.Text      -- Comment
  CommentIsPrivate         :: SearchField T.Text      -- Comment is private
  CommentTags              :: SearchField T.Text      -- Comment Tags
  Commenter                :: SearchField BzUserEmail -- Commenter
  Component                :: SearchField T.Text      -- Component
  Content                  :: SearchField T.Text      -- Content
  CreationDate             :: SearchField UTCTime     -- Creation date
  DaysElapsed              :: SearchField Int         -- Days since bug changed
  DependsOn                :: SearchField Int         -- Depends on
  EverConfirmed            :: SearchField Bool        -- Ever confirmed
  FlagRequestee            :: SearchField BzUserEmail -- Flag Requestee
  FlagSetter               :: SearchField BzUserEmail -- Flag Setter
  Flags                    :: SearchField T.Text      -- Flags
  Group                    :: SearchField T.Text      -- Group
  Keywords                 :: SearchField T.Text      -- Keywords
  Changed                  :: SearchField UTCTime     -- Changed
  CommentCount             :: SearchField Int         -- Number of Comments
  OperatingSystem          :: SearchField T.Text      -- OS
  Hardware                 :: SearchField T.Text      -- Hardware
  Priority                 :: SearchField T.Text      -- Priority
  Product                  :: SearchField T.Text      -- Product
  QaContact                :: SearchField BzUserEmail -- QA Contact
  Reporter                 :: SearchField BzUserEmail -- Reporter
  ReporterAccessible       :: SearchField Bool        -- Reporter accessible
  Resolution               :: SearchField T.Text      -- Resolution
  RestrictComments         :: SearchField Bool        -- Restrict Comments
  SeeAlso                  :: SearchField T.Text      -- See Also
  Severity                 :: SearchField T.Text      -- Severity
  Status                   :: SearchField T.Text      -- Status
  Whiteboard               :: SearchField T.Text      -- Whiteboard
  Summary                  :: SearchField T.Text      -- Summary
  Tags                     :: SearchField T.Text      -- Tags
  TargetMilestone          :: SearchField T.Text      -- Target Milestone
  TimeSinceAssigneeTouched :: SearchField Int         -- Time Since Assignee Touched
  BugURL                   :: SearchField T.Text      -- URL
  Version                  :: SearchField T.Text      -- Version
  Votes                    :: SearchField T.Text      -- Votes

searchFieldName :: SearchField a -> T.Text
searchFieldName Alias                    = "alias"
searchFieldName AssignedTo               = "assigned_to"
searchFieldName AttachmentCreator        = "attachments.submitter"
searchFieldName AttachmentData           = "attach_data.thedata"
searchFieldName AttachmentDescription    = "attachments.description"
searchFieldName AttachmentFilename       = "attachments.filename"
searchFieldName AttachmentIsObsolete     = "attachments.isobsolete"
searchFieldName AttachmentIsPatch        = "attachments.ispatch"
searchFieldName AttachmentIsPrivate      = "attachments.isprivate"
searchFieldName AttachmentMimetype       = "attachments.mimetype"
searchFieldName Blocks                   = "blocked"
searchFieldName BugId                    = "bug_id"
searchFieldName Cc                       = "cc"
searchFieldName CcListAccessible         = "cclist_accessible"
searchFieldName Classification           = "classification"
searchFieldName Comment                  = "longdesc"
searchFieldName CommentIsPrivate         = "longdescs.isprivate"
searchFieldName CommentTags              = "comment_tag"
searchFieldName Commenter                = "commenter"
searchFieldName Component                = "component"
searchFieldName Content                  = "content"
searchFieldName CreationDate             = "creation_ts"
searchFieldName DaysElapsed              = "days_elapsed"
searchFieldName DependsOn                = "dependson"
searchFieldName EverConfirmed            = "everconfirmed"
searchFieldName FlagRequestee            = "requestees.login_name"
searchFieldName FlagSetter               = "setters.login_name"
searchFieldName Flags                    = "flagtypes.name"
searchFieldName Group                    = "bug_group"
searchFieldName Keywords                 = "keywords"
searchFieldName Changed                  = "delta_ts"
searchFieldName CommentCount             = "longdescs.count"
searchFieldName OperatingSystem          = "op_sys"
searchFieldName Hardware                 = "rep_platform"
searchFieldName Priority                 = "priority"
searchFieldName Product                  = "product"
searchFieldName QaContact                = "qa_contact"
searchFieldName Reporter                 = "reporter"
searchFieldName ReporterAccessible       = "reporter_accessible"
searchFieldName Resolution               = "resolution"
searchFieldName RestrictComments         = "restrict_comments"
searchFieldName SeeAlso                  = "see_also"
searchFieldName Severity                 = "bug_severity"
searchFieldName Status                   = "bug_status"
searchFieldName Whiteboard               = "status_whiteboard"
searchFieldName Summary                  = "short_desc"
searchFieldName Tags                     = "tag"
searchFieldName TargetMilestone          = "target_milestone"
searchFieldName TimeSinceAssigneeTouched = "owner_idle_time"
searchFieldName BugURL                   = "bug_file_loc"
searchFieldName Version                  = "version"
searchFieldName Votes                    = "votes"

data SearchTerm where
  Equals                     :: FieldValue a => SearchField a -> a -> SearchTerm
  NotEquals                  :: FieldValue a => SearchField a -> a -> SearchTerm
  EqualsAnyString            :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsString             :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsStringMatchingCase :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  DoesNotContainString       :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsAnyString          :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsAllStrings         :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsNoneOfStrings      :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  RegexpMatches              :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  RegexpNotMatches           :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  LessThan                   :: FieldValue a => SearchField a -> a -> SearchTerm
  LessThanOrEqual            :: FieldValue a => SearchField a -> a -> SearchTerm
  GreaterThan                :: FieldValue a => SearchField a -> a -> SearchTerm
  GreaterThanOrEqual         :: FieldValue a => SearchField a -> a -> SearchTerm
  ContainsAnyWords           :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsAllWords           :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ContainsNoneOfWords        :: FieldValue a => SearchField a -> T.Text -> SearchTerm
  ChangedBefore              :: FieldValue a => SearchField a -> UTCTime -> SearchTerm
  ChangedAfter               :: FieldValue a => SearchField a -> UTCTime -> SearchTerm
  ChangedFrom                :: FieldValue a => SearchField a -> a -> SearchTerm
  ChangedTo                  :: FieldValue a => SearchField a -> a -> SearchTerm
  ChangedBy                  :: FieldValue a => SearchField a -> BzUserEmail -> SearchTerm
  ContentMatches             :: T.Text -> SearchTerm
  ContentNotMatches          :: T.Text -> SearchTerm
  IsEmpty                    :: FieldValue a => SearchField a -> SearchTerm
  IsNotEmpty                 :: FieldValue a => SearchField a -> SearchTerm

(.==.) :: FieldValue a => SearchField a -> a -> SearchTerm
(.==.) = Equals
infix 4 .==.

(./=.) :: FieldValue a => SearchField a -> a -> SearchTerm
(./=.) = NotEquals
infix 4 ./=.

(.<.) :: FieldValue a => SearchField a -> a -> SearchTerm
(.<.) = LessThan
infix 4 .<.

(.<=.) :: FieldValue a => SearchField a -> a -> SearchTerm
(.<=.) = LessThanOrEqual
infix 4 .<=.

(.>.) :: FieldValue a => SearchField a -> a -> SearchTerm
(.>.) = GreaterThan
infix 4 .>.

(.>=.) :: FieldValue a => SearchField a -> a -> SearchTerm
(.>=.) = GreaterThanOrEqual
infix 4 .>=.

(.=~.) :: FieldValue a => SearchField a -> T.Text -> SearchTerm
(.=~.) = RegexpMatches

(./=~.) :: FieldValue a => SearchField a -> T.Text -> SearchTerm
(./=~.) = RegexpNotMatches

contains :: FieldValue a => SearchField a -> T.Text -> SearchTerm
contains = ContainsString

data SearchExpr
  = And [SearchExpr]
  | Or [SearchExpr]
  | Not SearchExpr
  | Term SearchTerm

class SearchExprItem a where asSearchExpr :: a -> SearchExpr
instance SearchExprItem SearchExpr where asSearchExpr = id
instance SearchExprItem SearchTerm where asSearchExpr = Term

(.&&.) :: (SearchExprItem a, SearchExprItem b) => a -> b -> SearchExpr
(.&&.) a b = And [asSearchExpr a, asSearchExpr b]
infixr 3 .&&.

(.||.) :: (SearchExprItem a, SearchExprItem b) => a -> b -> SearchExpr
(.||.) a b = Or [asSearchExpr a, asSearchExpr b]
infixr 2 .||.

notE :: SearchExprItem a => a -> SearchExpr
notE a = Not . asSearchExpr $ a
infix 4 `notE`

taggedQueryPart :: Int -> Char -> T.Text -> QueryPart
taggedQueryPart t k v = (T.cons k . T.pack . show $ t, Just v)

termQuery :: FieldValue b => Int -> SearchField a -> T.Text -> b -> [QueryPart]
termQuery t f o v = [taggedQueryPart t 'f' (searchFieldName f),
                     taggedQueryPart t 'o' o,
                     taggedQueryPart t 'v' (fvAsText v)]

evalSearchTerm :: Int -> SearchTerm -> [QueryPart]
evalSearchTerm t (Equals field val)                     = termQuery t field "equals" val
evalSearchTerm t (NotEquals field val)                  = termQuery t field "notequals" val
evalSearchTerm t (EqualsAnyString field val)            = termQuery t field "anyexact" val
evalSearchTerm t (ContainsString field val)             = termQuery t field "substring" val
evalSearchTerm t (ContainsStringMatchingCase field val) = termQuery t field "casesubstring" val
evalSearchTerm t (DoesNotContainString field val)       = termQuery t field "notsubstring" val
evalSearchTerm t (ContainsAnyString field val)          = termQuery t field "anywordssubstr" val
evalSearchTerm t (ContainsAllStrings field val)         = termQuery t field "allwordssubstr" val
evalSearchTerm t (ContainsNoneOfStrings field val)      = termQuery t field "nowordssubstr" val
evalSearchTerm t (RegexpMatches field val)              = termQuery t field "regexp" val
evalSearchTerm t (RegexpNotMatches field val)           = termQuery t field "notregexp" val
evalSearchTerm t (LessThan field val)                   = termQuery t field "lessthan" val
evalSearchTerm t (LessThanOrEqual field val)            = termQuery t field "lessthaneq" val
evalSearchTerm t (GreaterThan field val)                = termQuery t field "greaterthan" val
evalSearchTerm t (GreaterThanOrEqual field val)         = termQuery t field "greaterthaneq" val
evalSearchTerm t (ContainsAnyWords field val)           = termQuery t field "anywords" val
evalSearchTerm t (ContainsAllWords field val)           = termQuery t field "allwords" val
evalSearchTerm t (ContainsNoneOfWords field val)        = termQuery t field "nowords" val
evalSearchTerm t (ChangedBefore field val)              = termQuery t field "changedbefore" val
evalSearchTerm t (ChangedAfter field val)               = termQuery t field "changedafter" val
evalSearchTerm t (ChangedFrom field val)                = termQuery t field "changedfrom" val
evalSearchTerm t (ChangedTo field val)                  = termQuery t field "changedto" val
evalSearchTerm t (ChangedBy field val)                  = termQuery t field "changedby" val
evalSearchTerm t (ContentMatches val)                   = termQuery t Content "matches" val
evalSearchTerm t (ContentNotMatches val)                = termQuery t Content "notmatches" val
evalSearchTerm t (IsEmpty field)                        = termQuery t field "isempty" ("" :: T.Text)
evalSearchTerm t (IsNotEmpty field)                     = termQuery t field "isnotempty" ("" :: T.Text)

evalSearchExpr :: SearchExpr -> [QueryPart]
evalSearchExpr e = snd $ evalSearchExpr' 1 e
  where
    evalExprGroup :: Int -> [SearchExpr] -> (Int, [QueryPart])
    evalExprGroup t es =
      let (subExprT, subExprQs) = foldl' evalSubExpr (t + 1, []) es
          qs = (taggedQueryPart t 'f' "OP") :
               (taggedQueryPart subExprT 'f' "CP") :
               subExprQs
      in (subExprT + 1, qs)

    evalSubExpr :: (Int, [QueryPart]) -> SearchExpr -> (Int, [QueryPart])
    evalSubExpr (t, qs) expr = let (nextT, qs') = evalSearchExpr' t expr
                               in  (nextT, qs ++ qs')

    evalSearchExpr' :: Int -> SearchExpr -> (Int, [QueryPart])
    evalSearchExpr' t (And es) = evalExprGroup t es

    evalSearchExpr' t (Or es) =
      let (groupT, groupQs) = evalExprGroup t es
          qs = (taggedQueryPart t 'j' "OR") : groupQs
      in (groupT + 1, qs)

    evalSearchExpr' t (Not es) =
      let (groupT, groupQs) = evalSearchExpr' t es
          qs = (taggedQueryPart t 'n' "1") : groupQs
      in (groupT + 1, qs)

    evalSearchExpr' t (Term term) = (t + 1, evalSearchTerm t term)

includeFields :: S.Set BugField -> QueryPart
includeFields fields = ("include_fields", Just $ names fields)
  where names = T.intercalate "," . map bugFieldName . S.toList

data SearchResult = SearchResult [Bug]
                    deriving (Eq, Show)

instance FromJSON SearchResult where
  parseJSON (Object v) = SearchResult <$> v .: "bugs"
  parseJSON _          = mzero
  
bzSearchBugsRequest :: BzContext -> S.Set BugField -> SearchExpr -> Request
bzSearchBugsRequest ctx include search = bzRequest ctx ["bug"] $ is : qs
  where
    is = includeFields $ BugFieldId `S.insert` include
    qs = evalSearchExpr search

searchBugs :: SearchExprItem a => BzContext -> S.Set BugField -> a -> IO [Bug]
searchBugs ctx fields search = runResourceT $ do
  let req = bzSearchBugsRequest ctx fields (asSearchExpr search)
  liftIO $ print $ requestUrl req
  response <- liftIO $ httpLbs req (bzManager ctx)
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg                  -> throw $ BugzillaException $ "JSON parse error: " ++ msg
    Right (SearchResult bugs) -> return bugs
