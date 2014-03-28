{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla
( assignedTo
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
--import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit (httpLbs, mkManagerSettings, newManager, parseUrl, Response(..))

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


apiQueryUrl :: T.Text -> [T.Text] -> [(T.Text, T.Text)] -> T.Text
apiQueryUrl server methodParts queryParts =
    T.concat ["https://", server, "/rest/", combineMethod methodParts, combineQuery queryParts]
  where
    combineMethod = T.intercalate "/"

    combineQuery [] = ""
    combineQuery ps = combineQuery' ps

    combineQuery' = T.cons '?'
                  . T.intercalate "&"
                  . map joinParam

    joinParam (k, v) = T.concat [k, "=", v]

--apiGetUrl :: T.Text -> [T.Text] -> T.Text
--apiGetUrl server methodParts = apiQueryUrl server methodParts []

assignedTo :: T.Text -> T.Text -> IO TL.Text
assignedTo server user = runResourceT $ do
  let url = apiQueryUrl server ["bug"] [("assigned_to", user), ("include_fields", bugFieldName Id)]
  liftIO $ print url
  req <- parseUrl . T.unpack $ url
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  manager <- liftIO $ newManager settings
  response <- liftIO $ httpLbs req manager
  case responseBody response of
    jsonResponse -> return $ TE.decodeUtf8 jsonResponse
