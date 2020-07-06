module Data.OpenAPI.V300.Validator.Request where

import Prelude
import Data.Foldable (fold)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (head, last, tail, init)
import Data.Either (Either, either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HttpTypes.V000 (HttpTypesMap, Method(..), Request(..))
import Data.List (List(..), (:), filter, fromFoldable, length)
import Data.List.NonEmpty (fromList, singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.OpenAPI.V300 as AST
import Data.OpenAPI.V300.Validator.Lens
  ( getPathParametersForPathAndMethod
  , validateJsonAgainstSchema
  )
import Data.String (split, Pattern(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), snd)
import Foreign (ForeignError)
import Simple.JSON (readJSON)

newtype RequestError
  = RequestError
  { method :: Maybe (NonEmptyList String)
  , protocol :: Maybe (NonEmptyList String)
  , host :: Maybe (NonEmptyList String)
  , url :: Maybe (NonEmptyList String)
  , path :: Maybe (NonEmptyList String)
  , pathname :: Maybe (NonEmptyList String)
  , query :: Maybe (HttpTypesMap (NonEmptyList String))
  , headers :: Maybe (HttpTypesMap (NonEmptyList String))
  , body :: Maybe (NonEmptyList String)
  , timestamp :: Maybe (NonEmptyList String)
  }

derive instance newtypeRequestError :: Newtype RequestError _

derive instance genericRequestError :: Generic RequestError _

derive newtype instance semigroupRequestError :: Semigroup RequestError

derive newtype instance monoidRequestError :: Monoid RequestError

derive newtype instance eqRequestError :: Eq RequestError

instance showRequestError :: Show RequestError where
  show = genericShow

hasMethod ∷ Method → AST.PathItem → Boolean
hasMethod POST (AST.PathItem { _post: (Just _) }) = true

hasMethod GET (AST.PathItem { _get: (Just _) }) = true

hasMethod DELETE (AST.PathItem { _delete: (Just _) }) = true

hasMethod PUT (AST.PathItem { _put: (Just _) }) = true

hasMethod HEAD (AST.PathItem { _head: (Just _) }) = true

hasMethod PATCH (AST.PathItem { _patch: (Just _) }) = true

hasMethod TRACE (AST.PathItem { _trace: (Just _) }) = true

hasMethod OPTIONS (AST.PathItem { _options: (Just _) }) = true

hasMethod _ _ = false

getViableMethodsForRequest ∷
  Request →
  List (Tuple String AST.PathItem) →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViableMethodsForRequest (Request { method }) paths =
  note
    ( RequestError
        ( mempty
            { method:
                Just
                  ( singleton $ "Method "
                      <> show method
                      <> " does not exist in OpenAPI schema"
                  )
            }
        )
    )
    (fromList (filter (hasMethod method <<< snd) paths))

------------- path splitter
splitPathAndRemoveEmpties ∷ String → List String
splitPathAndRemoveEmpties s = filter ((/=) "") (fromFoldable $ split (Pattern "/") s)

type MatchPathToOAITailRec
  = { acc ∷ RequestError, loai ∷ List String, linc ∷ List String }

middle ∷ String → String
middle s = maybe "" fromCharArray (go (toCharArray s))
  where
  go ∷ Array Char → Maybe (Array Char)
  go arr = tail arr >>= init >>= pure

schemaToMatcher ∷
  (String → RequestError) →
  AST.OpenAPIObject →
  Maybe (AST.ReferenceOr AST.Schema) →
  String →
  RequestError
schemaToMatcher _ o Nothing _ = mempty

schemaToMatcher errorFactory o (Just x) s =
  let
    v = validateJsonAgainstSchema errorFactory o x (AST.JString s)
  in
    if v == mempty then
      mempty
    else
      ( either
          (\e → errorFactory $ "Error while parsing JSON " <> show e)
          ( \j →
              let
                q = validateJsonAgainstSchema errorFactory o x j
              in
                if q == mempty then mempty else (v <> q)
          )
          (readJSON s ∷ Either (NonEmptyList ForeignError) AST.JSON)
      )

matchPathToOAIPath ∷ AST.OpenAPIObject → Tuple String AST.PathItem → Request → RequestError
matchPathToOAIPath o (Tuple p pi) (Request { path, method }) =
  maybe
    (RequestError $ mempty { path: Just $ singleton "Incoming path for match is nothing" })
    (\_p → tailRec go { acc: mempty, loai: (splitPathAndRemoveEmpties p), linc: (splitPathAndRemoveEmpties _p) })
    path
  where
  errorFactory =
    ( \s →
        RequestError
          $ mempty
              { path:
                  Just
                    ( singleton
                        $ "Error for path "
                        <> p
                        <> " @ "
                        <> show path
                        <> s
                    )
              }
    )

  mismatchError =
    Done
      ( RequestError
          $ mempty
              { path:
                  Just
                    ( singleton
                        $ "Paths are different lengths: "
                        <> p
                        <> " @ "
                        <> show path
                    )
              }
      )

  getMatcherFromSpec ∷ String → String → RequestError
  getMatcherFromSpec pt =
    let
      ca = toCharArray pt
    in
      -- this is a path param
      if head ca == Just '{' && last ca == Just '}' then
        let
          allPathParamsWithName =
            filter
              (\(AST.Parameter pm) → pm._name == middle pt)
              (getPathParametersForPathAndMethod o pi method)
        in
          -- there are no path params defined, so we accept anything
          if length allPathParamsWithName == 0 then
            defaultMatch
          else
            -- here, we go from the assumption that if there are multiple parameters that match, then they have
            -- an AND relationship, meaning the schema must match EVERY qualified parameter
            fold
              $ map
                  ( \(AST.Parameter pm) →
                      schemaToMatcher errorFactory o pm._schema
                  )
                  allPathParamsWithName
      else
        simpleStringMatch errorFactory pt

  go ∷ MatchPathToOAITailRec → Step MatchPathToOAITailRec RequestError
  go { acc, loai: Nil, linc: Nil } = Done acc

  go { acc, loai: (x : xs), linc: (y : ys) } =
    if length xs /= length ys then
      mismatchError
    else
      Loop { acc: acc <> (getMatcherFromSpec x y), loai: xs, linc: ys }

  -- needed to be complete
  -- we could have gotten rid of the if above, but the algorithm would run
  -- to the end, and we want it to fail fast
  go { acc, loai: _, linc: _ } = mismatchError

------------- matchers
defaultMatch ∷ ∀ a. a → RequestError
defaultMatch = const mempty

noMatch ∷ ∀ a. Show a ⇒ (String → RequestError) → a → RequestError
noMatch errorFactory s = const (errorFactory $ "Could not build any match for" <> show s) s

--   | tomatch == s = RequestError (mempty { path: Just $ singleton ("Could not match " <> tomatch <> " and " <> s) })
simpleStringMatch ∷ (String → RequestError) → String → String → RequestError
simpleStringMatch errorFactory tomatch s
  | tomatch == s = errorFactory $ "Could not match " <> tomatch <> " and " <> s
  | otherwise = mempty
