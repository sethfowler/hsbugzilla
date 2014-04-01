{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Search
( 
  -- * Search operators
  (.==.)
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

  -- * Search expressions
, Field (..)
, SearchExpression
) where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))

import Web.Bugzilla.Internal.Search
import Web.Bugzilla.Internal.Types

(.==.) :: FieldType a => Field a -> a -> SearchExpression
(.==.) = (Term .) . BinaryOp "equals"
infix 4 .==.

(./=.) :: FieldType a => Field a -> a -> SearchExpression
(./=.) = (Term .) . BinaryOp "notequals"
infix 4 ./=.

(.<.) :: FieldType a => Field a -> a -> SearchExpression
(.<.) = (Term .) . BinaryOp "lessthan"
infix 4 .<.

(.<=.) :: FieldType a => Field a -> a -> SearchExpression
(.<=.) = (Term .) . BinaryOp "lessthaneq"
infix 4 .<=.

(.>.) :: FieldType a => Field a -> a -> SearchExpression
(.>.) = (Term .) . BinaryOp "greaterthan"
infix 4 .>.

(.>=.) :: FieldType a => Field a -> a -> SearchExpression
(.>=.) = (Term .) . BinaryOp "greaterthaneq"
infix 4 .>=.

(.=~.) :: FieldType a => Field a -> a -> SearchExpression
(.=~.) = (Term .) . BinaryOp "regexp"

(./=~.) :: FieldType a => Field a -> a -> SearchExpression
(./=~.) = (Term .) . BinaryOp "notregexp"

equalsAny :: FieldType a => Field a -> [a] -> SearchExpression
equalsAny = (Term .) . BinaryOp "anyexact"

contains :: Field T.Text -> T.Text -> SearchExpression
contains = (Term .) . BinaryOp "substring"

containsCase :: Field T.Text -> T.Text -> SearchExpression
containsCase = (Term .) . BinaryOp "casesubstring"

containsAny :: Field T.Text -> [T.Text] -> SearchExpression
containsAny = (Term .) . BinaryOp "anywordssubstr"

containsAll :: Field T.Text -> [T.Text] -> SearchExpression
containsAll = (Term .) . BinaryOp "allwordssubstr"

changedBefore :: FieldType a => Field a -> UTCTime -> SearchExpression
changedBefore = (Term .) . BinaryOp "changedbefore"

changedAfter :: FieldType a => Field a -> UTCTime -> SearchExpression
changedAfter = (Term .) . BinaryOp "changedafter"

changedFrom :: FieldType a => Field a -> a -> SearchExpression
changedFrom = (Term .) . BinaryOp "changedfrom"

changedTo :: FieldType a => Field a -> a -> SearchExpression
changedTo = (Term .) . BinaryOp "changedto"

changedBy :: FieldType a => Field a -> UserEmail -> SearchExpression
changedBy = (Term .) . BinaryOp "changedby"

contentMatches :: T.Text -> SearchExpression
contentMatches = Term . BinaryOp "matches" ContentField

isEmpty :: FieldType a => Field a -> SearchExpression
isEmpty = Term . UnaryOp "isempty"

(.&&.) :: SearchExpression -> SearchExpression -> SearchExpression
(.&&.) a (And as) = And (a:as)
(.&&.) a b        = And [a, b]
infixr 3 .&&.

(.||.) :: SearchExpression -> SearchExpression -> SearchExpression
(.||.) a (Or as) = Or (a:as)
(.||.) a b       = Or [a, b]
infixr 2 .||.

not' :: SearchExpression -> SearchExpression
not' (Not a) = a
not' a       = Not a
