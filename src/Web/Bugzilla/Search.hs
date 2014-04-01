{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.Search
( Field (..)
, SearchExpr
, (.==.)
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
, searchBugs
, searchBugsWithLimit
) where

import Data.List
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.ISO8601 (formatISO8601)

import Web.Bugzilla
import Web.Bugzilla.Internal
import Web.Bugzilla.Internal.Types

class FieldType a where fvAsText :: a -> T.Text

instance FieldType T.Text where fvAsText = id
instance FieldType Int where fvAsText = T.pack . show
instance FieldType UTCTime where fvAsText = T.pack . formatISO8601

instance FieldType Bool where
  fvAsText True  = "true"
  fvAsText False = "false"

instance FieldType a => FieldType [a] where
  fvAsText = T.intercalate "," . map fvAsText

data SearchTerm where
  UnaryOp  :: FieldType a => T.Text -> Field a -> SearchTerm
  BinaryOp :: (FieldType a, FieldType b) => T.Text -> Field a -> b -> SearchTerm

(.==.) :: FieldType a => Field a -> a -> SearchExpr
(.==.) = (Term .) . BinaryOp "equals"
infix 4 .==.

(./=.) :: FieldType a => Field a -> a -> SearchExpr
(./=.) = (Term .) . BinaryOp "notequals"
infix 4 ./=.

(.<.) :: FieldType a => Field a -> a -> SearchExpr
(.<.) = (Term .) . BinaryOp "lessthan"
infix 4 .<.

(.<=.) :: FieldType a => Field a -> a -> SearchExpr
(.<=.) = (Term .) . BinaryOp "lessthaneq"
infix 4 .<=.

(.>.) :: FieldType a => Field a -> a -> SearchExpr
(.>.) = (Term .) . BinaryOp "greaterthan"
infix 4 .>.

(.>=.) :: FieldType a => Field a -> a -> SearchExpr
(.>=.) = (Term .) . BinaryOp "greaterthaneq"
infix 4 .>=.

(.=~.) :: FieldType a => Field a -> a -> SearchExpr
(.=~.) = (Term .) . BinaryOp "regexp"

(./=~.) :: FieldType a => Field a -> a -> SearchExpr
(./=~.) = (Term .) . BinaryOp "notregexp"

equalsAny :: FieldType a => Field a -> [a] -> SearchExpr
equalsAny = (Term .) . BinaryOp "anyexact"

contains :: Field T.Text -> T.Text -> SearchExpr
contains = (Term .) . BinaryOp "substring"

containsCase :: Field T.Text -> T.Text -> SearchExpr
containsCase = (Term .) . BinaryOp "casesubstring"

containsAny :: Field T.Text -> [T.Text] -> SearchExpr
containsAny = (Term .) . BinaryOp "anywordssubstr"

containsAll :: Field T.Text -> [T.Text] -> SearchExpr
containsAll = (Term .) . BinaryOp "allwordssubstr"

changedBefore :: FieldType a => Field a -> UTCTime -> SearchExpr
changedBefore = (Term .) . BinaryOp "changedbefore"

changedAfter :: FieldType a => Field a -> UTCTime -> SearchExpr
changedAfter = (Term .) . BinaryOp "changedafter"

changedFrom :: FieldType a => Field a -> a -> SearchExpr
changedFrom = (Term .) . BinaryOp "changedfrom"

changedTo :: FieldType a => Field a -> a -> SearchExpr
changedTo = (Term .) . BinaryOp "changedto"

changedBy :: FieldType a => Field a -> BzUserEmail -> SearchExpr
changedBy = (Term .) . BinaryOp "changedby"

contentMatches :: T.Text -> SearchExpr
contentMatches = Term . BinaryOp "matches" ContentField

isEmpty :: FieldType a => Field a -> SearchExpr
isEmpty = Term . UnaryOp "isempty"

data SearchExpr
  = And [SearchExpr]
  | Or [SearchExpr]
  | Not SearchExpr
  | Term SearchTerm

(.&&.) :: SearchExpr -> SearchExpr -> SearchExpr
(.&&.) a (And as) = And (a:as)
(.&&.) a b        = And [a, b]
infixr 3 .&&.

(.||.) :: SearchExpr -> SearchExpr -> SearchExpr
(.||.) a (Or as) = Or (a:as)
(.||.) a b       = Or [a, b]
infixr 2 .||.

not' :: SearchExpr -> SearchExpr
not' (Not a) = a
not' a       = Not a

taggedQueryPart :: Int -> Char -> T.Text -> QueryPart
taggedQueryPart t k v = (T.cons k . T.pack . show $ t, Just v)

termQuery :: FieldType b => Int -> Field a -> T.Text -> b -> [QueryPart]
termQuery t f o v = [taggedQueryPart t 'f' (searchFieldName f),
                     taggedQueryPart t 'o' o,
                     taggedQueryPart t 'v' (fvAsText v)]

evalSearchTerm :: Int -> SearchTerm -> [QueryPart]
evalSearchTerm t (UnaryOp op field)          = termQuery t field op ("" :: T.Text)
evalSearchTerm t (BinaryOp op field val)     = termQuery t field op val

evalSearchExpr :: SearchExpr -> [QueryPart]
evalSearchExpr e = snd $ evalSearchExpr' 1 e
  where
    evalExprGroup :: Int -> [SearchExpr] -> (Int, [QueryPart])
    evalExprGroup t es =
      let (subExprT, subExprQs) = foldl' evalSubExpr (t + 1, []) es
          qs = (taggedQueryPart t 'f' "OP") :
               (taggedQueryPart subExprT 'f' "CP") :
               subExprQs
      in (subExprT + 1, qs)

    evalSubExpr :: (Int, [QueryPart]) -> SearchExpr -> (Int, [QueryPart])
    evalSubExpr (t, qs) expr = let (nextT, qs') = evalSearchExpr' t expr
                               in  (nextT, qs ++ qs')

    evalSearchExpr' :: Int -> SearchExpr -> (Int, [QueryPart])
    evalSearchExpr' t (And es) = evalExprGroup t es

    evalSearchExpr' t (Or es) =
      let (groupT, groupQs) = evalExprGroup t es
          qs = (taggedQueryPart t 'j' "OR") : groupQs
      in (groupT + 1, qs)

    evalSearchExpr' t (Not es) =
      let (groupT, groupQs) = evalSearchExpr' t es
          qs = (taggedQueryPart t 'n' "1") : groupQs
      in (groupT + 1, qs)

    evalSearchExpr' t (Term term) = (t + 1, evalSearchTerm t term)

searchBugs :: BzSession -> SearchExpr -> IO [Bug]
searchBugs session search = do
  let searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] searchQuery
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  return bugs

searchBugsWithLimit :: BzSession -> Int -> Int -> SearchExpr -> IO [Bug]
searchBugsWithLimit session limit offset search = do
  let limitQuery = [("limit", Just $ fvAsText limit),
                    ("offset", Just $ fvAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (limitQuery ++ searchQuery)
  print $ requestUrl req
  (BugList bugs) <- sendBzRequest session req
  return bugs
