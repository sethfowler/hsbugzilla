{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Text as T

import Web.Bugzilla
import Web.Bugzilla.Search

main :: IO ()
main = withBzContext "bugzilla.mozilla.org" $ \ctx -> do
  putStrLn "Enter username: "
  user <- T.pack <$> getLine
  putStrLn "Enter password: "
  password <- T.pack <$> getLine
  session <- loginSession ctx user password
  --let search = AssignedTo .==. user
  let search = FlagRequestee .==. user .&&. Flags `contains` "needinfo"
  bugs <- searchBugs session search
  mapM_ print bugs
