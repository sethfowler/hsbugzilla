{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main = withBzContext "bugzilla.mozilla.org" $ \ctx -> do
  let user = "seth@mozilla.com"
  --let search = (AssignedTo `Equals` user)
  let search = (FlagRequestee `Equals` user)
  bugs <- searchBugs ctx (Set.singleton BugFieldFlags) search
  mapM_ print bugs
