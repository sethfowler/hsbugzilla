{-# LANGUAGE OverloadedStrings #-}

import Web.Bugzilla

main :: IO ()
main = withBzContext "bugzilla.mozilla.org" $ \ctx -> do
  bugs <- assignedTo ctx standardBugFields "seth@mozilla.com"
  mapM_ print bugs
