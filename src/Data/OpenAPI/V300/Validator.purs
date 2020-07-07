module Data.OpenAPI.V300.Validator
  ( getViablePathItemsForRequest
  , RequestError(..)
  , T_RequestError
  , getViablePathItemsForResponse
  , ResponseError(..)
  , T_ResponseError
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (head, last, tail, init)
import Data.Either (Either(..), either, note)
import Data.Foldable (fold, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HttpTypes.V000 (Header(..), HttpTypesMap(..), Method(..), Query(..), Request(..), Response(..))
import Data.Lens as L
import Data.List (List(..), (:), filter, fromFoldable, length)
import Data.List as List
import Data.List.NonEmpty (toList, fromList, singleton)
import Data.List.Types (NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.OpenAPI.V300 as AST
import Data.OpenAPI.V300.Validator.Lens (getHeaderParametersForPathAndMethod, getPathParametersForPathAndMethod, getQueryParametersForPathAndMethod, lensFromContentToReferenceOrSchemas, lensToHeadersForResponse, lensToRequestBodyForMethod, lensToResponseSchemasForMethod, validateJsonAgainstSchema)
import Data.String (split, Pattern(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (ForeignError)
import Record (set)
import Simple.JSON (readJSON)

log' :: forall a. Show a => a -> a
log' a =
  unsafePerformEffect do
    log $ show a
    pure a

log'' :: forall a. Eq a => a -> a -> a
log'' assumption a =
  unsafePerformEffect do
    log $ show (a == assumption)
    pure a

type T_RequestError
  = { method ∷ Maybe (NonEmptyList String)
    , protocol ∷ Maybe (NonEmptyList String)
    , host ∷ Maybe (NonEmptyList String)
    , url ∷ Maybe (NonEmptyList String)
    , path ∷ Maybe (NonEmptyList String)
    , pathname ∷ Maybe (NonEmptyList String)
    , query ∷ Maybe (HttpTypesMap (NonEmptyList String))
    , headers ∷ Maybe (HttpTypesMap (NonEmptyList String))
    , body ∷ Maybe (NonEmptyList String)
    , timestamp ∷ Maybe (NonEmptyList String)
    }

newtype RequestError
  = RequestError T_RequestError

derive instance newtypeRequestError ∷ Newtype RequestError _

derive instance genericRequestError ∷ Generic RequestError _

derive newtype instance semigroupRequestError ∷ Semigroup RequestError

derive newtype instance monoidRequestError ∷ Monoid RequestError

derive newtype instance eqRequestError ∷ Eq RequestError

instance showRequestError ∷ Show RequestError where
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

getViablePathItemsForRequestUsingMethod ∷
  Request →
  List (Tuple String AST.PathItem) →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForRequestUsingMethod (Request { method }) paths =
  note
    ( RequestError
        $ ( set (SProxy ∷ SProxy "method")
              ( Just
                  ( singleton $ "Method "
                      <> show method
                      <> " does not exist in OpenAPI schema"
                  )
              )
              (mempty ∷ T_RequestError)
          )
    )
    (fromList (filter (hasMethod method <<< snd) paths))

getViablePathItemsFor' ∷
  ∀ a m.
  Monoid m ⇒
  Eq m ⇒
  (AST.OpenAPIObject → Tuple String AST.PathItem → a → m) →
  AST.OpenAPIObject →
  a →
  List (Tuple String AST.PathItem) →
  Either m (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsFor' d o r paths =
  let
    matches =
      map
        ( \x →
            Tuple x $ d o x r
        )
        paths
  in
    note
      (fold $ map snd matches)
      ( fromList
          $ map fst (filter ((==) mempty <<< snd) matches)
      )

getViablePathItemsForRequestUsingPath ∷
  AST.OpenAPIObject →
  Request →
  List (Tuple String AST.PathItem) →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForRequestUsingPath =
  getViablePathItemsFor'
    matchPathToOAIPath

getViablePathItemsForRequestUsingHeaders ∷
  AST.OpenAPIObject →
  Request →
  List (Tuple String AST.PathItem) →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForRequestUsingHeaders =
  getViablePathItemsFor'
    ( \oo tpi (Request { method, headers, path }) →
        matchHeaderOrQueryToOAIHeaderOrQuery
          (headersToKV <$> headers)
          getHeaderParametersForPathAndMethod
          ( \k s →
              ( RequestError
                  $ ( set (SProxy ∷ SProxy "headers")
                        ( Just
                            $ HttpTypesMap
                                ( Map.singleton k
                                    (singleton s)
                                )
                        )
                        (mempty ∷ T_RequestError)
                    )
              )
          )
          oo
          tpi
          method
    )

getViablePathItemsForRequestUsingRequestBody ∷
  AST.OpenAPIObject →
  Request →
  List (Tuple String AST.PathItem) →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForRequestUsingRequestBody =
  getViablePathItemsFor'
    matchRequestBodyToOAIRequestBody

getViablePathItemsForRequestUsingQuery ∷
  AST.OpenAPIObject →
  Request →
  List (Tuple String AST.PathItem) →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForRequestUsingQuery =
  getViablePathItemsFor'
    ( \oo tpi (Request { method, path, query }) →
        matchHeaderOrQueryToOAIHeaderOrQuery
          (queryToKV <$> query)
          getQueryParametersForPathAndMethod
          ( \k s →
              ( RequestError
                  $ ( set (SProxy ∷ SProxy "query")
                        ( Just
                            $ HttpTypesMap
                                ( Map.singleton k
                                    (singleton s)
                                )
                        )
                        (mempty ∷ T_RequestError)
                    )
              )
          )
          oo
          tpi
          method
    )

------------- path splitter
splitPathAndRemoveEmpties ∷ String → List String
splitPathAndRemoveEmpties s =
  filter
    ((/=) "")
    (fromFoldable $ split (Pattern "/") s)

type MatchPathToOAITailRec
  = { acc ∷ RequestError
    , loai ∷ List String
    , linc ∷ List String
    }

type MatchSchemaListToOAITailRec a
  = { acc ∷ a
    , s ∷ String
    , schemas ∷ List (AST.ReferenceOr AST.Schema)
    }

type MatchHeaderOrQueryToOAITailRec a
  = { acc ∷ a
    , p ∷ List AST.Parameter
    }

type MatchHeaderOrQueryToOAIInnerTailRec a
  = { acc ∷ a
    , p ∷ AST.Parameter
    , kv ∷ List (Tuple String String)
    }

middle ∷ String → String
middle s = maybe "" fromCharArray (go (toCharArray s))
  where
  go ∷ Array Char → Maybe (Array Char)
  go arr = tail arr >>= init >>= pure

----------------------
schemaToMatcher ∷
  ∀ a.
  Monoid a ⇒
  Eq a ⇒
  (String → a) →
  AST.OpenAPIObject →
  Maybe (AST.ReferenceOr AST.Schema) →
  String →
  a
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


------------- matchers
defaultMatch ∷ ∀ a b. Monoid b ⇒ a → b
defaultMatch = const mempty

simpleStringMatch ∷ (String → RequestError) → String → String → RequestError
simpleStringMatch errorFactory tomatch s
  | tomatch /= s = errorFactory $ "Could not match " <> tomatch <> " and " <> s
  | otherwise = mempty

matchPathToOAIPath ∷ AST.OpenAPIObject → Tuple String AST.PathItem → Request → RequestError
matchPathToOAIPath o (Tuple p pi) (Request { pathname, method }) =
  maybe
    ( RequestError
        $ ( set (SProxy ∷ SProxy "path")
              (Just (singleton "Incoming path for match is nothing"))
              (mempty ∷ T_RequestError)
          )
    )
    (\_p → tailRec go { acc: mempty, loai: (splitPathAndRemoveEmpties p), linc: (splitPathAndRemoveEmpties _p) })
    pathname
  where
  errorFactory =
    ( \s →
        RequestError
          $ ( set (SProxy ∷ SProxy "path")
                ( Just
                    ( singleton
                        $ "Error for path "
                        <> p
                        <> " @ "
                        <> show pathname
                        <> s
                    )
                )
                (mempty ∷ T_RequestError)
            )
    )

  mismatchError =
    Done
      ( RequestError
          $ ( set (SProxy ∷ SProxy "path")
                ( Just
                    ( singleton
                        $ "Paths are different lengths: "
                        <> p
                        <> " @ "
                        <> show pathname
                    )
                )
                (mempty ∷ T_RequestError)
            )
      )

  getMatcherFromSpec ∷ String → String → RequestError
  getMatcherFromSpec pt =
    let
      ca = toCharArray pt
    in
      -- this is a path param
      if head ca == Just '{' && last ca == Just '}' then
        let
          allParams =
            filter
              (\(AST.Parameter pm) → pm._name == middle pt)
              (getPathParametersForPathAndMethod o pi method)
        in
          if length allParams == 0 then
            -- we do not penalize the spec if there are
            -- no params
            defaultMatch
          else
            -- here, we go from the assumption that if there are multiple parameters that match, then they have
            -- an AND relationship, meaning the schema must match EVERY qualified parameter
            fold
              $ map
                  ( \(AST.Parameter pm) →
                      schemaToMatcher errorFactory o pm._schema
                  )
                  allParams
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

--------------------------
--------------------------
--------------------------
--------------------------
queryToKV ∷ HttpTypesMap Query → List (Tuple String String)
queryToKV (HttpTypesMap m) = fold $ map toTSS (Map.toUnfoldable m ∷ List (Tuple String Query))
  where
  toTSS ∷ Tuple String Query → List (Tuple String String)
  toTSS (Tuple k (StringQuery s)) = List.singleton (Tuple k s)

  toTSS (Tuple k (ArrayQuery a)) = List.fromFoldable $ map (Tuple k) a

headersToKV ∷ HttpTypesMap Header → List (Tuple String String)
headersToKV (HttpTypesMap m) = fold $ map toTSS (Map.toUnfoldable m ∷ List (Tuple String Header))
  where
  toTSS ∷ Tuple String Header → List (Tuple String String)
  toTSS (Tuple k (StringHeader s)) = List.singleton (Tuple k s)

  toTSS (Tuple k (ArrayHeader a)) = List.fromFoldable $ map (Tuple k) a

matchHeaderOrQueryToOAIHeaderOrQuery ∷
  forall m.
  Monoid m =>
  Eq m =>
  Maybe (List (Tuple String String)) →
  ( AST.OpenAPIObject →
    AST.PathItem →
    Method →
    List AST.Parameter
  ) →
  (String → String → m) →
  AST.OpenAPIObject →
  Tuple String AST.PathItem →
  Method →
  m
matchHeaderOrQueryToOAIHeaderOrQuery mltss getParams errorFactory o (Tuple p pi) method =
  tailRec go
    { acc: mempty
    , p: allParams
    }
  where
  allQH = fromMaybe Nil mltss

  allParams = getParams o pi method

  innerGo ∷ (MatchHeaderOrQueryToOAIInnerTailRec m) → Step (MatchHeaderOrQueryToOAIInnerTailRec m) m
  innerGo { acc, kv: Nil } = Done acc

  innerGo { acc
  , kv: (Tuple k v) : xs
  , p: param@(AST.Parameter pm)
  } =
    Loop
      { acc:
          acc
            <> ( schemaToMatcher
                  (errorFactory k)
                  o
                  pm._schema
                  v
              )
      , kv: xs
      , p: param
      }

  go ∷ (MatchHeaderOrQueryToOAITailRec m) → Step (MatchHeaderOrQueryToOAITailRec m) m
  go { acc, p: Nil } = Done acc

  go { acc, p: (pm@(AST.Parameter pm') : xs) } =
    let
      l = filter (\(Tuple k v) → k == pm'._name) allQH
    in
      if (pm'._required == Just true && length l == 0) then
        Loop
          { acc:
              acc
                <> ( errorFactory pm'._name $ "Required parameter "
                      <> pm'._name
                      <> " is missing."
                  )
          , p: xs
          }
      else
        Loop
          { acc:
              acc
                <> ( tailRec innerGo
                      { acc: mempty
                      , kv: l
                      , p: pm
                      }
                  )
          , p: xs
          }

requestBodyError' ∷ String → String → RequestError
requestBodyError' path s =
  RequestError
    $ ( set (SProxy ∷ SProxy "body")
          ( Just
              $ singleton
                  ("At path" <> path <> " @@ " <> s)
          )
          (mempty ∷ T_RequestError)
      )

-- todo, this is really close to response body. merge?
matchRequestBodyToOAIRequestBody ∷ AST.OpenAPIObject → Tuple String AST.PathItem → Request → RequestError
matchRequestBodyToOAIRequestBody o (Tuple p pi) (Request { path, method, body }) =
  maybe
    mempty
    ( \(AST.RequestBody rb) →
        maybe
          (if rb._required == Just true then bodyError "Could not find JSON for request body schema" else mempty)
          ( \s →
              tailRec
                go
                { acc: mempty
                , s
                , schemas:
                    L.toListOf
                      lensFromContentToReferenceOrSchemas
                      rb._content
                }
          )
          body
    )
    (L.preview (lensToRequestBodyForMethod o method) pi)
  where
  bodyError = requestBodyError' (fromMaybe "" path)

  go ∷ (MatchSchemaListToOAITailRec RequestError) → Step (MatchSchemaListToOAITailRec RequestError) RequestError
  go { acc, schemas: Nil } = Done acc

  go { acc, s, schemas: (x : xs) } =
    let
      v = schemaToMatcher bodyError o (Just x) s
    in
      if v == mempty then
        Done mempty
      else
        Loop { acc: acc <> v, schemas: xs, s }

getViablePathItemsForRequest ∷
  AST.OpenAPIObject →
  Request →
  Either RequestError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForRequest o@(AST.OpenAPIObject { _paths: (AST.OAIMap oo) }) r =  -- todo 
  foldl
    (\a b → a >>= b o r <<< toList)
    ( getViablePathItemsForRequestUsingMethod
        r
        (Map.toUnfoldable oo)
    )
    [ getViablePathItemsForRequestUsingPath
    , getViablePathItemsForRequestUsingHeaders
    , getViablePathItemsForRequestUsingQuery
    , getViablePathItemsForRequestUsingRequestBody
    ]

-----------------------------
type T_ResponseError
  = { statusCode :: Maybe (NonEmptyList String)
    , body :: Maybe (NonEmptyList String)
    , headers :: Maybe (HttpTypesMap (NonEmptyList String))
    , timestamp :: Maybe (NonEmptyList String)
    }

newtype ResponseError
  = ResponseError T_ResponseError

derive instance newtypeResponseError ∷ Newtype ResponseError _

derive instance genericResponseError ∷ Generic ResponseError _

derive newtype instance semigroupResponseError ∷ Semigroup ResponseError

derive newtype instance monoidResponseError ∷ Monoid ResponseError

derive newtype instance eqResponseError ∷ Eq ResponseError

instance showResponseError ∷ Show ResponseError where
  show = genericShow

responseBodyError' ∷ String → String → ResponseError
responseBodyError' path s =
  ResponseError
    $ ( set (SProxy ∷ SProxy "body")
          ( Just
              $ singleton
                  ("At path" <> path <> " @@ " <> s)
          )
          (mempty ∷ T_ResponseError)
      )

-- todo, this is really close to request body. merge?
matchResponseBodyToOAIResponseBody ∷ AST.OpenAPIObject → Tuple String AST.PathItem → Tuple Request Response → ResponseError
matchResponseBodyToOAIResponseBody o (Tuple p pi) (Tuple (Request { path, method }) (Response { statusCode, body })) =
  maybe
    mempty
    ( \s →
        tailRec
          go
          { acc: mempty
          , s
          , schemas: (L.toListOf (lensToResponseSchemasForMethod o method (show statusCode)) pi)
          }
    )
    body
  where
  bodyError = responseBodyError' (fromMaybe "" path)

  go ∷ (MatchSchemaListToOAITailRec ResponseError) → Step (MatchSchemaListToOAITailRec ResponseError) ResponseError
  go { acc, schemas: Nil } = Done acc

  go { acc, s, schemas: (x : xs) } =
    let
      v = schemaToMatcher bodyError o (Just x) s
    in
      if v == mempty then
        Done mempty
      else
        Loop { acc: acc <> v, schemas: xs, s }

getViablePathItemsForResponseUsingResponseBody ∷
  AST.OpenAPIObject →
  Request →
  Response →
  List (Tuple String AST.PathItem) →
  Either ResponseError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForResponseUsingResponseBody o rq rs l =
  getViablePathItemsFor'
    matchResponseBodyToOAIResponseBody
    o
    (Tuple rq rs)
    l

getViablePathItemsForResponseUsingHeaders ∷
  AST.OpenAPIObject →
  Request →
  Response →
  List (Tuple String AST.PathItem) →
  Either ResponseError (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForResponseUsingHeaders o rq rs l =
  getViablePathItemsFor'
    ( \oo tpi (Tuple (Request { method }) (Response { statusCode, headers })) →
        matchHeaderOrQueryToOAIHeaderOrQuery
          (headersToKV <$> headers)
          (\ooo p m → L.toListOf (lensToHeadersForResponse ooo m (show statusCode)) p)
          ( \k s →
              ( ResponseError
                  $ ( set (SProxy ∷ SProxy "headers")
                        ( Just
                            $ HttpTypesMap
                                ( Map.singleton k
                                    (singleton s)
                                )
                        )
                        (mempty ∷ T_ResponseError)
                    )
              )
          )
          oo
          tpi
          method
    )
    o
    (Tuple rq rs)
    l

getViablePathItemsForResponse ∷
  AST.OpenAPIObject →
  Request →
  Response →
  Either (Either RequestError ResponseError) (NonEmptyList (Tuple String AST.PathItem))
getViablePathItemsForResponse o@(AST.OpenAPIObject { _paths: (AST.OAIMap oo) }) rq rs = go (getViablePathItemsForRequest o rq)
  where
  go ∷ Either RequestError (NonEmptyList (Tuple String AST.PathItem)) → Either (Either RequestError ResponseError) (NonEmptyList (Tuple String AST.PathItem))
  go (Left r) = (Left (Left r))

  go (Right l) =
    gooo
      $ foldl
          (\a b → a >>= b o rq rs <<< toList)
          (pure l)
          [ getViablePathItemsForResponseUsingResponseBody
          , getViablePathItemsForResponseUsingHeaders
          ]

  gooo ∷ Either ResponseError (NonEmptyList (Tuple String AST.PathItem)) → Either (Either RequestError ResponseError) (NonEmptyList (Tuple String AST.PathItem))
  gooo (Left r) = (Left (Right r))

  gooo (Right l) = (Right l)
