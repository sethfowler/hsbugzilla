{-# LANGUAGE OverloadedStrings #-}

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main = withBzContext "bugzilla.mozilla.org" $ \ctx -> do
  let user = "seth@mozilla.com"
  --let search = AssignedTo .==. user
  let search = FlagRequestee .==. user .&&. Flags `contains` "needinfo"
  bugs <- searchBugs ctx search
  mapM_ print bugs
