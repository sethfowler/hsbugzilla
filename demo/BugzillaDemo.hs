{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import System.Environment (getArgs)
import System.IO

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main = dispatch Nothing Nothing =<< getArgs

dispatch :: Maybe UserEmail -> Maybe BugzillaServer -> [String] -> IO ()
dispatch Nothing s ("--login" : user : as)    = dispatch (Just $ T.pack user) s as
dispatch l Nothing ("--server" : server : as) = dispatch l (Just $ T.pack server) as
dispatch l s ["--assigned-to", user]         = withBz l s $ doAssignedTo (T.pack user)
dispatch l s ["--requests", user]            = withBz l s $ doRequests (T.pack user)
dispatch l s ["--history", bug]              = withBz l s $ doHistory (read bug)
dispatch _ _ _                                = usage

usage :: IO ()
usage = hPutStrLn stderr "Use --login, --server, --assigned-to, --requests, or --history"

withBz :: Maybe UserEmail -> Maybe BugzillaServer -> (BugzillaSession -> IO ()) -> IO ()
withBz mLogin mServer f = withBugzillaContext server $ \ctx ->
  case mLogin of
    Just login -> do hPutStrLn stderr "Enter password: "
                     password <- T.pack <$> getLine
                     mSession <- loginSession ctx login password
                     case mSession of
                       Just session -> f session
                       Nothing      -> do hPutStrLn stderr "Login failed. Falling back to anonymous session."
                                          f $ anonymousSession ctx
    Nothing -> f $ anonymousSession ctx
  where
    server = fromMaybe "bugzilla.mozilla.org" mServer
                     
  
doAssignedTo :: UserEmail -> BugzillaSession -> IO ()
doAssignedTo user session = do
    let search = AssignedToField .==. user
    bugs <- searchBugs session search
    mapM_ showBug bugs
  where
    showBug (Bug {..}) = putStrLn $ show bugId ++ ": " ++ show bugSummary
                                 ++ " [" ++ T.unpack bugStatus ++ ": " ++ T.unpack bugResolution ++ "] Updated: "
                                 ++ show bugLastChangeTime

doRequests :: UserEmail -> BugzillaSession -> IO ()
doRequests user session = do
    let needinfoSearch = FlagRequesteeField .==. user .&&. FlagsField `contains` "needinfo"
    needinfoBugs <- searchBugs session needinfoSearch
    mapM_ showNeedinfo needinfoBugs

    let reviewSearch = FlagRequesteeField .==. user .&&.
                       (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
    reviewBugs <- map bugId <$> searchBugs session reviewSearch
    forM_ reviewBugs $ \rBugId -> do
      attachments <- getAttachments session rBugId
      mapM_ showReview $ filter (any hasReviewFlag . attachmentFlags) attachments
      mapM_ showFeedback $ filter (any hasFeedbackFlag . attachmentFlags) attachments

  where
    showNeedinfo (Bug {..}) = do
      let flags = filter hasNeedinfoFlag bugFlags
      forM_ flags $ \flag ->
        putStrLn $ "[NEEDINFO] " ++ show bugId ++ ": " ++ show bugSummary
                ++ " (" ++ show (flagSetter flag) ++ " " ++ show (flagCreationDate flag) ++ ")"

    showReview (Attachment {..}) =
      putStrLn $ "[REVIEW] " ++ show attachmentBugId ++ ": " ++ show attachmentSummary
              ++ " (" ++ show attachmentCreator ++ " " ++ show attachmentCreationTime ++ ")"

    showFeedback (Attachment {..}) =
      putStrLn $ "[FEEDBACK] " ++ show attachmentBugId ++ ": " ++ show attachmentSummary
              ++ " (" ++ show attachmentCreator ++ " " ++ show attachmentCreationTime ++ ")"

    hasNeedinfoFlag f = flagRequestee f == Just user && flagName f == "needinfo"
    hasReviewFlag f   = flagRequestee f == Just user && flagName f == "review"
    hasFeedbackFlag f = flagRequestee f == Just user && flagName f == "feedback"

doHistory :: BugId -> BugzillaSession -> IO ()
doHistory bug session = do
    comments <- getComments session bug
    history <- getHistory session bug
    let recentEventsRev = takeRecent 10 (reverse comments) (reverse $ historyEntries history)
    mapM_ putStrLn (reverse recentEventsRev)
  where
    takeRecent 0 _ _ = []
    takeRecent n (c:cs) (e:es)
      | commentCreationTime c `diffUTCTime` historyEntryWhen e >= 0 = showComment c : takeRecent (n - 1) cs (e:es)
      | otherwise                                                   = showEvent e : takeRecent (n - 1) (c:cs) es
    takeRecent n cs@(_:_) [] = map showComment $ take n cs
    takeRecent n [] es@(_:_) = map showEvent $ take n es
    takeRecent _ [] []       = []

    showComment (Comment {..}) = "(" ++ show commentId ++ ") " ++ T.unpack commentCreator ++ "  " ++ show commentCreationTime
                              ++ "\n" ++ (unlines . map ("  " ++) . lines . T.unpack $ commentText)

    showEvent (HistoryEntry {..}) = "(" ++ T.unpack historyEntryWho ++ ")\n" ++ concatMap showChange historyEntryChanges

    showChange (TextFieldChange f (Modification r a aid)) = "  " ++ showField f ++ ": " ++ showMod r ++ " -> " ++ showMod a ++ showAid aid ++ "\n"
    showChange (ListFieldChange f (Modification r a aid)) = "  " ++ showField f ++ ": " ++ showMod r ++ " -> " ++ showMod a ++ showAid aid ++ "\n"
    showChange (IntFieldChange f (Modification r a aid))  = "  " ++ showField f ++ ": " ++ showMod r ++ " -> " ++ showMod a ++ showAid aid ++ "\n"
    showChange (TimeFieldChange f (Modification r a aid)) = "  " ++ showField f ++ ": " ++ showMod r ++ " -> " ++ showMod a ++ showAid aid ++ "\n"
    showChange (BoolFieldChange f (Modification r a aid)) = "  " ++ showField f ++ ": " ++ showMod r ++ " -> " ++ showMod a ++ showAid aid ++ "\n"

    showMod :: Show a => Maybe a -> String
    showMod (Just v) = show v
    showMod Nothing  = "___"

    showAid :: Maybe AttachmentId -> String
    showAid (Just aid) = " (Attachment " ++ show aid ++ ")"
    showAid Nothing    = ""

    showField = T.unpack . fieldName
