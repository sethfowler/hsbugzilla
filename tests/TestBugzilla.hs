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
  mSession <- loginSession ctx user password
  case mSession of
    Just session -> runTest session user
    Nothing      -> do putStrLn "Login failed. Running test with anonymous session."
                       runTest (AnonymousSession ctx) user

runTest :: BzSession -> T.Text -> IO ()
runTest session user = do
  --let search = AssignedTo .==. user
  {-
  let search = FlagRequestee .==. user .&&. Flags `contains` "needinfo"
  bugs <- searchBugs session search
  mapM_ print bugs
  -}
  as <- getAttachment session 8397620
  print as
