{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Search
( SearchField (..)
, SearchExpr
, (.==.)
, (./=.)
, (.<.)
, (.<=.)
, (.>.)
, (.>=.)
, (.=~.)
, (./=~.)
, equalsAny
, contains
, containsCase
, containsAny
, containsAll
, changedBefore
, changedAfter
, changedFrom
, changedTo
, changedBy
, contentMatches
, isEmpty
, (.&&.)
, (.||.)
, not'
, searchBugs
, searchBugsWithLimit
) where

import Data.List
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.ISO8601 (formatISO8601)

import Web.Bugzilla
import Web.Bugzilla.Internal

class FieldType a where fvAsText :: a -> T.Text

instance FieldType T.Text where fvAsText = id
instance FieldType Int where fvAsText = T.pack . show
instance FieldType UTCTime where fvAsText = T.pack . formatISO8601

instance FieldType Bool where
  fvAsText True  = "true"
  fvAsText False = "false"

instance FieldType a => FieldType [a] where
  fvAsText = T.intercalate "," . map fvAsText

data SearchField a where
  AliasField                    :: SearchField T.Text      -- Alias
  AssignedToField               :: SearchField BzUserEmail -- Assignee
  AttachmentCreatorField        :: SearchField BzUserEmail -- Attachment creator
  AttachmentDataField           :: SearchField T.Text      -- Attachment data
  AttachmentDescriptionField    :: SearchField T.Text      -- Attachment description
  AttachmentFilenameField       :: SearchField T.Text      -- Attachment filename
  AttachmentIsObsoleteField     :: SearchField Bool        -- Attachment is obsolete
  AttachmentIsPatchField        :: SearchField Bool        -- Attachment is patch
  AttachmentIsPrivateField      :: SearchField Bool        -- Attachment is private
  AttachmentMimetypeField       :: SearchField T.Text      -- Attachment mime type
  BlocksField                   :: SearchField Int         -- Blocks
  BugIdField                    :: SearchField Int         -- Bug ID
  CcField                       :: SearchField BzUserEmail -- CC
  CcListAccessibleField         :: SearchField Bool        -- CC list accessible
  ClassificationField           :: SearchField T.Text      -- Classification
  CommentField                  :: SearchField T.Text      -- Comment
  CommentIsPrivateField         :: SearchField T.Text      -- Comment is private
  CommentTagsField              :: SearchField T.Text      -- Comment Tags
  CommenterField                :: SearchField BzUserEmail -- Commenter
  ComponentField                :: SearchField T.Text      -- Component
  ContentField                  :: SearchField T.Text      -- Content
  CreationDateField             :: SearchField UTCTime     -- Creation date
  DaysElapsedField              :: SearchField Int         -- Days since bug changed
  DependsOnField                :: SearchField Int         -- Depends on
  EverConfirmedField            :: SearchField Bool        -- Ever confirmed
  FlagRequesteeField            :: SearchField BzUserEmail -- Flag Requestee
  FlagSetterField               :: SearchField BzUserEmail -- Flag Setter
  FlagsField                    :: SearchField T.Text      -- Flags
  GroupField                    :: SearchField T.Text      -- Group
  KeywordsField                 :: SearchField T.Text      -- Keywords
  ChangedField                  :: SearchField UTCTime     -- Changed
  CommentCountField             :: SearchField Int         -- Number of Comments
  OperatingSystemField          :: SearchField T.Text      -- OS
  HardwareField                 :: SearchField T.Text      -- Hardware
  PriorityField                 :: SearchField T.Text      -- Priority
  ProductField                  :: SearchField T.Text      -- Product
  QaContactField                :: SearchField BzUserEmail -- QA Contact
  ReporterField                 :: SearchField BzUserEmail -- Reporter
  ReporterAccessibleField       :: SearchField Bool        -- Reporter accessible
  ResolutionField               :: SearchField T.Text      -- Resolution
  RestrictCommentsField         :: SearchField Bool        -- Restrict Comments
  SeeAlsoField                  :: SearchField T.Text      -- See Also
  SeverityField                 :: SearchField T.Text      -- Severity
  StatusField                   :: SearchField T.Text      -- Status
  WhiteboardField               :: SearchField T.Text      -- Whiteboard
  SummaryField                  :: SearchField T.Text      -- Summary
  TagsField                     :: SearchField T.Text      -- Tags
  TargetMilestoneField          :: SearchField T.Text      -- Target Milestone
  TimeSinceAssigneeTouchedField :: SearchField Int         -- Time Since Assignee Touched
  BugURLField                   :: SearchField T.Text      -- URL
  VersionField                  :: SearchField T.Text      -- Version
  VotesField                    :: SearchField T.Text      -- Votes

searchFieldName :: SearchField a -> T.Text
searchFieldName AliasField                    = "alias"
searchFieldName AssignedToField               = "assigned_to"
searchFieldName AttachmentCreatorField        = "attachments.submitter"
searchFieldName AttachmentDataField           = "attach_data.thedata"
searchFieldName AttachmentDescriptionField    = "attachments.description"
searchFieldName AttachmentFilenameField       = "attachments.filename"
searchFieldName AttachmentIsObsoleteField     = "attachments.isobsolete"
searchFieldName AttachmentIsPatchField        = "attachments.ispatch"
searchFieldName AttachmentIsPrivateField      = "attachments.isprivate"
searchFieldName AttachmentMimetypeField       = "attachments.mimetype"
searchFieldName BlocksField                   = "blocked"
searchFieldName BugIdField                    = "bug_id"
searchFieldName CcField                       = "cc"
searchFieldName CcListAccessibleField         = "cclist_accessible"
searchFieldName ClassificationField           = "classification"
searchFieldName CommentField                  = "longdesc"
searchFieldName CommentIsPrivateField         = "longdescs.isprivate"
searchFieldName CommentTagsField              = "comment_tag"
searchFieldName CommenterField                = "commenter"
searchFieldName ComponentField                = "component"
searchFieldName ContentField                  = "content"
searchFieldName CreationDateField             = "creation_ts"
searchFieldName DaysElapsedField              = "days_elapsed"
searchFieldName DependsOnField                = "dependson"
searchFieldName EverConfirmedField            = "everconfirmed"
searchFieldName FlagRequesteeField            = "requestees.login_name"
searchFieldName FlagSetterField               = "setters.login_name"
searchFieldName FlagsField                    = "flagtypes.name"
searchFieldName GroupField                    = "bug_group"
searchFieldName KeywordsField                 = "keywords"
searchFieldName ChangedField                  = "delta_ts"
searchFieldName CommentCountField             = "longdescs.count"
searchFieldName OperatingSystemField          = "op_sys"
searchFieldName HardwareField                 = "rep_platform"
searchFieldName PriorityField                 = "priority"
searchFieldName ProductField                  = "product"
searchFieldName QaContactField                = "qa_contact"
searchFieldName ReporterField                 = "reporter"
searchFieldName ReporterAccessibleField       = "reporter_accessible"
searchFieldName ResolutionField               = "resolution"
searchFieldName RestrictCommentsField         = "restrict_comments"
searchFieldName SeeAlsoField                  = "see_also"
searchFieldName SeverityField                 = "bug_severity"
searchFieldName StatusField                   = "bug_status"
searchFieldName WhiteboardField               = "status_whiteboard"
searchFieldName SummaryField                  = "short_desc"
searchFieldName TagsField                     = "tag"
searchFieldName TargetMilestoneField          = "target_milestone"
searchFieldName TimeSinceAssigneeTouchedField = "owner_idle_time"
searchFieldName BugURLField                   = "bug_file_loc"
searchFieldName VersionField                  = "version"
searchFieldName VotesField                    = "votes"

data SearchTerm where
  UnaryOp  :: FieldType a => T.Text -> SearchField a -> SearchTerm
  BinaryOp :: (FieldType a, FieldType b) => T.Text -> SearchField a -> b -> SearchTerm

(.==.) :: FieldType a => SearchField a -> a -> SearchExpr
(.==.) = (Term .) . BinaryOp "equals"
infix 4 .==.

(./=.) :: FieldType a => SearchField a -> a -> SearchExpr
(./=.) = (Term .) . BinaryOp "notequals"
infix 4 ./=.

(.<.) :: FieldType a => SearchField a -> a -> SearchExpr
(.<.) = (Term .) . BinaryOp "lessthan"
infix 4 .<.

(.<=.) :: FieldType a => SearchField a -> a -> SearchExpr
(.<=.) = (Term .) . BinaryOp "lessthaneq"
infix 4 .<=.

(.>.) :: FieldType a => SearchField a -> a -> SearchExpr
(.>.) = (Term .) . BinaryOp "greaterthan"
infix 4 .>.

(.>=.) :: FieldType a => SearchField a -> a -> SearchExpr
(.>=.) = (Term .) . BinaryOp "greaterthaneq"
infix 4 .>=.

(.=~.) :: FieldType a => SearchField a -> a -> SearchExpr
(.=~.) = (Term .) . BinaryOp "regexp"

(./=~.) :: FieldType a => SearchField a -> a -> SearchExpr
(./=~.) = (Term .) . BinaryOp "notregexp"

equalsAny :: FieldType a => SearchField a -> [a] -> SearchExpr
equalsAny = (Term .) . BinaryOp "anyexact"

contains :: SearchField T.Text -> T.Text -> SearchExpr
contains = (Term .) . BinaryOp "substring"

containsCase :: SearchField T.Text -> T.Text -> SearchExpr
containsCase = (Term .) . BinaryOp "casesubstring"

containsAny :: SearchField T.Text -> [T.Text] -> SearchExpr
containsAny = (Term .) . BinaryOp "anywordssubstr"

containsAll :: SearchField T.Text -> [T.Text] -> SearchExpr
containsAll = (Term .) . BinaryOp "allwordssubstr"

changedBefore :: FieldType a => SearchField a -> UTCTime -> SearchExpr
changedBefore = (Term .) . BinaryOp "changedbefore"

changedAfter :: FieldType a => SearchField a -> UTCTime -> SearchExpr
changedAfter = (Term .) . BinaryOp "changedafter"

changedFrom :: FieldType a => SearchField a -> a -> SearchExpr
changedFrom = (Term .) . BinaryOp "changedfrom"

changedTo :: FieldType a => SearchField a -> a -> SearchExpr
changedTo = (Term .) . BinaryOp "changedto"

changedBy :: FieldType a => SearchField a -> BzUserEmail -> SearchExpr
changedBy = (Term .) . BinaryOp "changedby"

contentMatches :: T.Text -> SearchExpr
contentMatches = Term . BinaryOp "matches" ContentField

isEmpty :: FieldType a => SearchField a -> SearchExpr
isEmpty = Term . UnaryOp "isempty"

data SearchExpr
  = And [SearchExpr]
  | Or [SearchExpr]
  | Not SearchExpr
  | Term SearchTerm

(.&&.) :: SearchExpr -> SearchExpr -> SearchExpr
(.&&.) a (And as) = And (a:as)
(.&&.) a b        = And [a, b]
infixr 3 .&&.

(.||.) :: SearchExpr -> SearchExpr -> SearchExpr
(.||.) a (Or as) = Or (a:as)
(.||.) a b       = Or [a, b]
infixr 2 .||.

not' :: SearchExpr -> SearchExpr
not' (Not a) = a
not' a       = Not a

taggedQueryPart :: Int -> Char -> T.Text -> QueryPart
taggedQueryPart t k v = (T.cons k . T.pack . show $ t, Just v)

termQuery :: FieldType b => Int -> SearchField a -> T.Text -> b -> [QueryPart]
termQuery t f o v = [taggedQueryPart t 'f' (searchFieldName f),
                     taggedQueryPart t 'o' o,
                     taggedQueryPart t 'v' (fvAsText v)]

evalSearchTerm :: Int -> SearchTerm -> [QueryPart]
evalSearchTerm t (UnaryOp op field)          = termQuery t field op ("" :: T.Text)
evalSearchTerm t (BinaryOp op field val)     = termQuery t field op val

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

searchBugs :: BzSession -> SearchExpr -> IO [Bug]
searchBugs session search = do
  let searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] searchQuery
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  return bugs

searchBugsWithLimit :: BzSession -> Int -> Int -> SearchExpr -> IO [Bug]
searchBugsWithLimit session limit offset search = do
  let limitQuery = [("limit", Just $ fvAsText limit),
                    ("offset", Just $ fvAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (limitQuery ++ searchQuery)
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  return bugs
