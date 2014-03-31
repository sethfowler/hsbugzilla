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
contentMatches = Term . BinaryOp "matches" Content

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
