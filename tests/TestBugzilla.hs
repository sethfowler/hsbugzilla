{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy.IO (putStrLn)
import Prelude hiding (putStrLn)

import Web.Bugzilla

main :: IO ()
main = putStrLn =<< assignedTo "bugzilla.mozilla.org" "seth@mozilla.com"
