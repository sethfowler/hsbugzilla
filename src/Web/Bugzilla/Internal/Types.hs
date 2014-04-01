{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Internal.Types
( BzUserEmail
, Field (..)
, searchFieldName
) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))

type BzUserEmail = T.Text

data Field a where
  AliasField                    :: Field T.Text           -- Alias
  AssignedToField               :: Field BzUserEmail      -- Assignee
  AttachmentCreatorField        :: Field BzUserEmail      -- Attachment creator
  AttachmentDataField           :: Field T.Text           -- Attachment data
  AttachmentDescriptionField    :: Field T.Text           -- Attachment description
  AttachmentFilenameField       :: Field T.Text           -- Attachment filename
  AttachmentIsObsoleteField     :: Field Bool             -- Attachment is obsolete
  AttachmentIsPatchField        :: Field Bool             -- Attachment is patch
  AttachmentIsPrivateField      :: Field Bool             -- Attachment is private
  AttachmentMimetypeField       :: Field T.Text           -- Attachment mime type
  BlocksField                   :: Field Int              -- Blocks
  BugIdField                    :: Field Int              -- Bug ID
  CcField                       :: Field BzUserEmail      -- CC
  CcListAccessibleField         :: Field Bool             -- CC list accessible
  ClassificationField           :: Field T.Text           -- Classification
  CommentField                  :: Field T.Text           -- Comment
  CommentIsPrivateField         :: Field T.Text           -- Comment is private
  CommentTagsField              :: Field T.Text           -- Comment Tags
  CommenterField                :: Field BzUserEmail      -- Commenter
  ComponentField                :: Field T.Text           -- Component
  ContentField                  :: Field T.Text           -- Content
  CreationDateField             :: Field UTCTime          -- Creation date
  DaysElapsedField              :: Field Int              -- Days since bug changed
  DependsOnField                :: Field Int              -- Depends on
  EverConfirmedField            :: Field Bool             -- Ever confirmed
  FlagRequesteeField            :: Field BzUserEmail      -- Flag Requestee
  FlagSetterField               :: Field BzUserEmail      -- Flag Setter
  FlagsField                    :: Field T.Text           -- Flags
  GroupField                    :: Field T.Text           -- Group
  KeywordsField                 :: Field [T.Text]         -- Keywords
  ChangedField                  :: Field UTCTime          -- Changed
  CommentCountField             :: Field Int              -- Number of Comments
  OperatingSystemField          :: Field T.Text           -- OS
  HardwareField                 :: Field T.Text           -- Hardware
  PriorityField                 :: Field T.Text           -- Priority
  ProductField                  :: Field T.Text           -- Product
  QaContactField                :: Field BzUserEmail      -- QA Contact
  ReporterField                 :: Field BzUserEmail      -- Reporter
  ReporterAccessibleField       :: Field Bool             -- Reporter accessible
  ResolutionField               :: Field T.Text           -- Resolution
  RestrictCommentsField         :: Field Bool             -- Restrict Comments
  SeeAlsoField                  :: Field T.Text           -- See Also
  SeverityField                 :: Field T.Text           -- Severity
  StatusField                   :: Field T.Text           -- Status
  WhiteboardField               :: Field T.Text           -- Whiteboard
  SummaryField                  :: Field T.Text           -- Summary
  TagsField                     :: Field T.Text           -- Tags
  TargetMilestoneField          :: Field T.Text           -- Target Milestone
  TimeSinceAssigneeTouchedField :: Field Int              -- Time Since Assignee Touched
  BugURLField                   :: Field T.Text           -- URL
  VersionField                  :: Field T.Text           -- Version
  VotesField                    :: Field T.Text           -- Votes
  CustomField                   :: T.Text -> Field T.Text -- (Custom fields)

instance Eq (Field a) where
  (CustomField a) == (CustomField b) = a == b
  (CustomField _) == _               = False
  _ == (CustomField _)               = False
  a == b                             = searchFieldName a == searchFieldName b

instance Show (Field a) where
  show AliasField                    = "AliasField"
  show AssignedToField               = "AssignedToField"
  show AttachmentCreatorField        = "AttachmentCreatorField"
  show AttachmentDataField           = "AttachmentDataField"
  show AttachmentDescriptionField    = "AttachmentDescriptionField"
  show AttachmentFilenameField       = "AttachmentFilenameField"
  show AttachmentIsObsoleteField     = "AttachmentIsObsoleteField"
  show AttachmentIsPatchField        = "AttachmentIsPatchField"
  show AttachmentIsPrivateField      = "AttachmentIsPrivateField"
  show AttachmentMimetypeField       = "AttachmentMimetypeField"
  show BlocksField                   = "BlocksField"
  show BugIdField                    = "BugIdField"
  show CcField                       = "CcField"
  show CcListAccessibleField         = "CcListAccessibleField"
  show ClassificationField           = "ClassificationField"
  show CommentField                  = "CommentField"
  show CommentIsPrivateField         = "CommentIsPrivateField"
  show CommentTagsField              = "CommentTagsField"
  show CommenterField                = "CommenterField"
  show ComponentField                = "ComponentField"
  show ContentField                  = "ContentField"
  show CreationDateField             = "CreationDateField"
  show DaysElapsedField              = "DaysElapsedField"
  show DependsOnField                = "DependsOnField"
  show EverConfirmedField            = "EverConfirmedField"
  show FlagRequesteeField            = "FlagRequesteeField"
  show FlagSetterField               = "FlagSetterField"
  show FlagsField                    = "FlagsField"
  show GroupField                    = "GroupField"
  show KeywordsField                 = "KeywordsField"
  show ChangedField                  = "ChangedField"
  show CommentCountField             = "CommentCountField"
  show OperatingSystemField          = "OperatingSystemField"
  show HardwareField                 = "HardwareField"
  show PriorityField                 = "PriorityField"
  show ProductField                  = "ProductField"
  show QaContactField                = "QaContactField"
  show ReporterField                 = "ReporterField"
  show ReporterAccessibleField       = "ReporterAccessibleField"
  show ResolutionField               = "ResolutionField"
  show RestrictCommentsField         = "RestrictCommentsField"
  show SeeAlsoField                  = "SeeAlsoField"
  show SeverityField                 = "SeverityField"
  show StatusField                   = "StatusField"
  show WhiteboardField               = "WhiteboardField"
  show SummaryField                  = "SummaryField"
  show TagsField                     = "TagsField"
  show TargetMilestoneField          = "TargetMilestoneField"
  show TimeSinceAssigneeTouchedField = "TimeSinceAssigneeTouchedField"
  show BugURLField                   = "BugURLField"
  show VersionField                  = "VersionField"
  show VotesField                    = "VotesField"
  show (CustomField name)            = "CustomField " ++ show name

searchFieldName :: Field a -> T.Text
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
searchFieldName (CustomField name)            = name
