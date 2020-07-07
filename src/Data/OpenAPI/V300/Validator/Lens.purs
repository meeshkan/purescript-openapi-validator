module Data.OpenAPI.V300.Validator.Lens where

import Prelude
import Control.Monad.Rec.Class (tailRec, Step(..))
import Data.Array (find, notElem)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.HttpTypes.V000 (Method(..))
import Data.Int as Int
import Data.Lens (Forget)
import Data.Lens as L
import Data.Lens.Record as LR
import Data.List (List(..), filter, fromFoldable, last, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe.First (First)
import Data.Monoid.Endo (Endo)
import Data.OpenAPI.V300 as AST
import Data.Profunctor.Choice (class Choice)
import Data.String (Pattern(..), length, split)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, uncurry)
import Foreign (ForeignError)
import Simple.JSON (readJSON, writeJSON)

down ∷ ∀ a b o. Tuple (a → b) (b → Maybe a) → (Choice o ⇒ o a a → o b b)
down = uncurry L.prism'

lastOfJSONPointer ∷ String → String
lastOfJSONPointer s =
  fromMaybe ""
    ( last
        $ filter
            ((/=) "")
            (fromFoldable $ split (Pattern "/") s)
    )

referenceOrPrism' ∷
  ∀ a o.
  Choice o ⇒
  L.Wander o ⇒
  (String → L.Forget (First (AST.ReferenceOr a)) (AST.ReferenceOr a) (AST.ReferenceOr a) → L.Forget (First (AST.ReferenceOr a)) AST.OpenAPIObject AST.OpenAPIObject) →
  AST.OpenAPIObject →
  (o a a → o (AST.ReferenceOr a) (AST.ReferenceOr a))
referenceOrPrism' f obj = L.prism' AST.RealDeal referenceOrPrism''
  where
  referenceOrPrism'' ∷ AST.ReferenceOr a → Maybe a
  referenceOrPrism'' (AST.RealDeal rd) = Just rd

  referenceOrPrism'' (AST.Ref (AST.Reference { _ref: s })) = maybe Nothing referenceOrPrism'' (L.preview (f $ lastOfJSONPointer s) obj)

--- ugggggh
referenceOrPrismT' ∷
  ∀ a o.
  Choice o ⇒
  L.Wander o ⇒
  (String → L.Forget (First (AST.ReferenceOr a)) (AST.ReferenceOr a) (AST.ReferenceOr a) → L.Forget (First (AST.ReferenceOr a)) AST.OpenAPIObject AST.OpenAPIObject) →
  AST.OpenAPIObject →
  (o (Tuple String a) (Tuple String a) → o (Tuple String (AST.ReferenceOr a)) (Tuple String (AST.ReferenceOr a)))
referenceOrPrismT' f obj = L.prism' (\(Tuple k v) → Tuple k $ AST.RealDeal v) referenceOrPrismT''
  where
  referenceOrPrismT'' ∷ Tuple String (AST.ReferenceOr a) → Maybe (Tuple String a)
  referenceOrPrismT'' (Tuple s (AST.RealDeal rd)) = Just (Tuple s rd)

  referenceOrPrismT'' (Tuple k (AST.Ref (AST.Reference { _ref: s }))) = maybe Nothing (\v → referenceOrPrismT'' $ Tuple k v) (L.preview (f $ lastOfJSONPointer s) obj)

referenceOrSchemaPrism ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → (o AST.Schema AST.Schema → o (AST.ReferenceOr AST.Schema) (AST.ReferenceOr AST.Schema))
referenceOrSchemaPrism = referenceOrPrism' lensToReferenceOrSchema

referenceOrHeaderPrism ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → (o AST.Header AST.Header → o (AST.ReferenceOr AST.Header) (AST.ReferenceOr AST.Header))
referenceOrHeaderPrism = referenceOrPrism' lensToReferenceOrHeader

referenceOrHeaderPrismT ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → (o (Tuple String AST.Header) (Tuple String AST.Header) → o (Tuple String (AST.ReferenceOr AST.Header)) (Tuple String (AST.ReferenceOr AST.Header)))
referenceOrHeaderPrismT = referenceOrPrismT' lensToReferenceOrHeader

referenceOrParameterPrism ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → (o AST.Parameter AST.Parameter → o (AST.ReferenceOr AST.Parameter) (AST.ReferenceOr AST.Parameter))
referenceOrParameterPrism = referenceOrPrism' lensToReferenceOrParameter

referenceOrRequestBodyPrism ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → (o AST.RequestBody AST.RequestBody → o (AST.ReferenceOr AST.RequestBody) (AST.ReferenceOr AST.RequestBody))
referenceOrRequestBodyPrism = referenceOrPrism' lensToReferenceOrRequestBody

referenceOrResponsePrism ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → (o AST.Response AST.Response → o (AST.ReferenceOr AST.Response) (AST.ReferenceOr AST.Response))
referenceOrResponsePrism = referenceOrPrism' lensToReferenceOrResponse

lensToComponents ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o AST.Components AST.Components → o AST.OpenAPIObject AST.OpenAPIObject
lensToComponents =
  down AST._OpenAPIObject
    <<< LR.prop (SProxy ∷ SProxy "_components")
    <<< L._Just

lensToComponent ∷
  ∀ a o.
  Choice o ⇒
  L.Wander o ⇒
  ( o (Maybe (AST.OAIMap (AST.ReferenceOr a))) (Maybe (AST.OAIMap (AST.ReferenceOr a))) →
    o AST.T_Components AST.T_Components
  ) →
  o (AST.OAIMap (AST.ReferenceOr a)) (AST.OAIMap (AST.ReferenceOr a)) → o AST.OpenAPIObject AST.OpenAPIObject
lensToComponent p =
  lensToComponents
    <<< down AST._Components
    <<< p
    <<< L._Just

lensToSchemas ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o (AST.OAIMap (AST.ReferenceOr AST.Schema)) (AST.OAIMap (AST.ReferenceOr AST.Schema)) → o AST.OpenAPIObject AST.OpenAPIObject
lensToSchemas = lensToComponent $ LR.prop (SProxy ∷ SProxy "_schemas")

lensToRequestBodies ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o (AST.OAIMap (AST.ReferenceOr AST.RequestBody)) (AST.OAIMap (AST.ReferenceOr AST.RequestBody)) → o AST.OpenAPIObject AST.OpenAPIObject
lensToRequestBodies = lensToComponent $ LR.prop (SProxy ∷ SProxy "_requestBodies")

lensToParameters ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o (AST.OAIMap (AST.ReferenceOr AST.Parameter)) (AST.OAIMap (AST.ReferenceOr AST.Parameter)) → o AST.OpenAPIObject AST.OpenAPIObject
lensToParameters = lensToComponent $ LR.prop (SProxy ∷ SProxy "_parameters")

lensToResponses ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o (AST.OAIMap (AST.ReferenceOr AST.Response)) (AST.OAIMap (AST.ReferenceOr AST.Response)) → o AST.OpenAPIObject AST.OpenAPIObject
lensToResponses = lensToComponent $ LR.prop (SProxy ∷ SProxy "_responses")

lensToHeaders ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o (AST.OAIMap (AST.ReferenceOr AST.Header)) (AST.OAIMap (AST.ReferenceOr AST.Header)) → o AST.OpenAPIObject AST.OpenAPIObject
lensToHeaders = lensToComponent $ LR.prop (SProxy ∷ SProxy "_headers")

mapIso :: forall k v. Ord k => L.Iso (Map.Map k v) (Map.Map k v) (List (Tuple k v)) (List (Tuple k v))
mapIso = L.iso Map.toUnfoldable Map.fromFoldable

headerIso :: L.Iso (Tuple String AST.Header) (Tuple String AST.Header) AST.Parameter AST.Parameter
headerIso =
  L.iso
    ( \( Tuple
          k
          ( AST.Header
            { _description
          , _required
          , _deprecated
          , _allowEmptyValue
          , _style
          , _explode
          , _allowReserved
          , _schema
          , _content
          , _example
          , _examples
          , _x
          }
        )
      ) →
        AST.Parameter
          { _in: "header"
          , _name: k
          , _description
          , _required
          , _deprecated
          , _allowEmptyValue
          , _style
          , _explode
          , _allowReserved
          , _schema
          , _content
          , _example
          , _examples
          , _x
          }
    )
    ( \( AST.Parameter
          { _in
        , _name
        , _description
        , _required
        , _deprecated
        , _allowEmptyValue
        , _style
        , _explode
        , _allowReserved
        , _schema
        , _content
        , _example
        , _examples
        , _x
        }
      ) →
        ( Tuple _name
            $ AST.Header
                { _description
                , _required
                , _deprecated
                , _allowEmptyValue
                , _style
                , _explode
                , _allowReserved
                , _schema
                , _content
                , _example
                , _examples
                , _x
                }
        )
    )

lensToReferenceOrA ∷ ∀ a o. Choice o ⇒ L.Wander o ⇒ String → o (AST.ReferenceOr a) (AST.ReferenceOr a) → o (AST.OAIMap (AST.ReferenceOr a)) (AST.OAIMap (AST.ReferenceOr a))
lensToReferenceOrA s =
  down AST._OAIMap
    <<< mapIso
    <<< L.traversed
    <<< L.filtered (eq s <<< fst)
    <<< L._2

lensToReferenceOrSchema ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ String → o (AST.ReferenceOr AST.Schema) (AST.ReferenceOr AST.Schema) → o AST.OpenAPIObject AST.OpenAPIObject
lensToReferenceOrSchema s = lensToSchemas <<< lensToReferenceOrA s

lensToReferenceOrRequestBody ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ String → o (AST.ReferenceOr AST.RequestBody) (AST.ReferenceOr AST.RequestBody) → o AST.OpenAPIObject AST.OpenAPIObject
lensToReferenceOrRequestBody s = lensToRequestBodies <<< lensToReferenceOrA s

lensToReferenceOrParameter ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ String → o (AST.ReferenceOr AST.Parameter) (AST.ReferenceOr AST.Parameter) → o AST.OpenAPIObject AST.OpenAPIObject
lensToReferenceOrParameter s = lensToParameters <<< lensToReferenceOrA s

lensToReferenceOrResponse ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ String → o (AST.ReferenceOr AST.Response) (AST.ReferenceOr AST.Response) → o AST.OpenAPIObject AST.OpenAPIObject
lensToReferenceOrResponse s = lensToResponses <<< lensToReferenceOrA s

lensToReferenceOrHeader ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ String → o (AST.ReferenceOr AST.Header) (AST.ReferenceOr AST.Header) → o AST.OpenAPIObject AST.OpenAPIObject
lensToReferenceOrHeader s = lensToHeaders <<< lensToReferenceOrA s

lensToPathItem ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o AST.PathItem AST.PathItem → o AST.OpenAPIObject AST.OpenAPIObject
lensToPathItem =
  down AST._OpenAPIObject
    <<< LR.prop (SProxy ∷ SProxy "_paths")
    <<< down AST._OAIMap
    <<< mapIso
    <<< L.traversed
    <<< L._2

methodToLens ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ Method → o (Maybe AST.Operation) (Maybe AST.Operation) → o AST.T_PathItem AST.T_PathItem
methodToLens = case _ of
  POST → LR.prop (SProxy ∷ SProxy "_post")
  GET → LR.prop (SProxy ∷ SProxy "_get")
  DELETE → LR.prop (SProxy ∷ SProxy "_delete")
  PUT → LR.prop (SProxy ∷ SProxy "_put")
  HEAD → LR.prop (SProxy ∷ SProxy "_head")
  PATCH → LR.prop (SProxy ∷ SProxy "_patch")
  TRACE → LR.prop (SProxy ∷ SProxy "_trace")
  OPTIONS → LR.prop (SProxy ∷ SProxy "_options")
  -- no CONNECT, so we give an empty lens
  _ → L.lens (const Nothing) const

type ParametersRecord r
  = { _parameters ∷
        Maybe (Array (AST.ReferenceOr AST.Parameter))
    | r
    }

lensToParameterFromTypeWithParameters ∷
  ∀ o r.
  Choice o ⇒
  L.Wander o ⇒
  AST.OpenAPIObject →
  o AST.Parameter AST.Parameter →
  o (ParametersRecord r) (ParametersRecord r)
lensToParameterFromTypeWithParameters obj =
  LR.prop (SProxy ∷ SProxy "_parameters")
    <<< L._Just
    <<< L.traversed
    <<< (referenceOrParameterPrism obj)

lensToParametersForPath ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → o AST.Parameter AST.Parameter → o AST.PathItem AST.PathItem
lensToParametersForPath obj =
  down AST._PathItem
    <<< lensToParameterFromTypeWithParameters obj

lensToParametersForMethod ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → Method → o AST.Parameter AST.Parameter → o AST.PathItem AST.PathItem
lensToParametersForMethod obj m =
  down AST._PathItem
    <<< methodToLens m
    <<< L._Just
    <<< down AST._Operation
    <<< lensToParameterFromTypeWithParameters obj

lensToHeadersForResponse ∷
  ∀ o.
  Choice o ⇒
  L.Wander o ⇒
  AST.OpenAPIObject →
  Method →
  String →
  o AST.Parameter AST.Parameter →
  o AST.PathItem AST.PathItem
lensToHeadersForResponse obj m s =
  lensToResponseForMethod obj m s
    <<< down AST._Response
    <<< LR.prop (SProxy ∷ SProxy "_headers")
    <<< L._Just
    <<< down AST._OAIMap
    <<< mapIso
    <<< L.traversed
    <<< (referenceOrHeaderPrismT obj)
    <<< headerIso

lensToRequestBodyForMethod ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → Method → o AST.RequestBody AST.RequestBody → o AST.PathItem AST.PathItem
lensToRequestBodyForMethod obj m =
  down AST._PathItem
    <<< methodToLens m
    <<< L._Just
    <<< down AST._Operation
    <<< LR.prop (SProxy ∷ SProxy "_requestBody")
    <<< L._Just
    <<< referenceOrRequestBodyPrism obj

lensToResponsesForMethod ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → Method → o (AST.OAIMap (AST.ReferenceOr AST.Response)) (AST.OAIMap (AST.ReferenceOr AST.Response)) → o AST.PathItem AST.PathItem
lensToResponsesForMethod obj m =
  down AST._PathItem
    <<< methodToLens m
    <<< L._Just
    <<< down AST._Operation
    <<< LR.prop (SProxy ∷ SProxy "_responses")

lensToResponseKeysForMethod ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → Method → o String String → o AST.PathItem AST.PathItem
lensToResponseKeysForMethod obj m =
  lensToResponsesForMethod obj m
    <<< down AST._OAIMap
    <<< mapIso
    <<< L.traversed
    <<< L._1

lensToResponseForMethod ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → Method → String → o AST.Response AST.Response → o AST.PathItem AST.PathItem
lensToResponseForMethod obj m s =
  lensToResponsesForMethod obj m
    <<< down AST._OAIMap
    <<< mapIso
    <<< L.traversed
    <<< L.filtered (\(Tuple k v) → k == "default" || k == s)
    <<< L._2
    <<< referenceOrResponsePrism obj

lensToResponseSchemasForMethod ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ AST.OpenAPIObject → Method → String → o (AST.ReferenceOr AST.Schema) (AST.ReferenceOr AST.Schema) → o AST.PathItem AST.PathItem
lensToResponseSchemasForMethod obj m s =
  lensToResponseForMethod obj m s
    <<< down AST._Response
    <<< LR.prop (SProxy ∷ SProxy "_content")
    <<< L._Just
    <<< lensFromContentToReferenceOrSchemas

lensFromContentToReferenceOrSchemas ∷
  ∀ o.
  Choice o ⇒
  L.Wander o ⇒
  o (AST.ReferenceOr AST.Schema) (AST.ReferenceOr AST.Schema) →
  o (AST.OAIMap AST.MediaType) (AST.OAIMap AST.MediaType)
lensFromContentToReferenceOrSchemas =
  down AST._OAIMap
    <<< mapIso
    <<< L.traversed
    <<< L._2
    <<< down AST._MediaType
    <<< LR.prop (SProxy ∷ SProxy "_schema")
    <<< L._Just

lensToPathParameter ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o AST.Parameter AST.Parameter → o AST.Parameter AST.Parameter
lensToPathParameter = L.filtered ((==) "path" <<< _._in <<< \(AST.Parameter p) → p)

lensToHeaderParameter ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o AST.Parameter AST.Parameter → o AST.Parameter AST.Parameter
lensToHeaderParameter = L.filtered ((==) "header" <<< _._in <<< \(AST.Parameter p) → p)

lensToQueryParameter ∷ ∀ o. Choice o ⇒ L.Wander o ⇒ o AST.Parameter AST.Parameter → o AST.Parameter AST.Parameter
lensToQueryParameter = L.filtered ((==) "query" <<< _._in <<< \(AST.Parameter p) → p)

getParametersForPathAndMethod' ∷ (Forget (Endo Function (List AST.Parameter)) AST.Parameter AST.Parameter → Forget (Endo Function (List AST.Parameter)) AST.Parameter AST.Parameter) → AST.OpenAPIObject → AST.PathItem → Method → List AST.Parameter
getParametersForPathAndMethod' l obj pi m =
  fold
    $ map
        (\q → L.toListOf (q <<< l) pi)
        (lensToParametersForPath obj : lensToParametersForMethod obj m : Nil)

getPathParametersForPathAndMethod ∷ AST.OpenAPIObject → AST.PathItem → Method → List AST.Parameter
getPathParametersForPathAndMethod = getParametersForPathAndMethod' lensToPathParameter

getQueryParametersForPathAndMethod ∷ AST.OpenAPIObject → AST.PathItem → Method → List AST.Parameter
getQueryParametersForPathAndMethod = getParametersForPathAndMethod' lensToQueryParameter

getHeaderParametersForPathAndMethod ∷ AST.OpenAPIObject → AST.PathItem → Method → List AST.Parameter
getHeaderParametersForPathAndMethod = getParametersForPathAndMethod' lensToHeaderParameter

eff ∷ ∀ a. (String → a) → String → String → a
eff f s0 s1 = f (s0 <> s1)

confirmProperty ∷ ∀ a. Monoid a ⇒ Eq a ⇒ (String → a) → AST.OpenAPIObject → Boolean → AST.ReferenceOr AST.Schema → Maybe AST.JSON → a
confirmProperty errorFactory _ true _ Nothing = errorFactory $ "The property is marked as required, but not present in the JSON."

confirmProperty errorFactory _ false _ Nothing = mempty

confirmProperty errorFactory o _ schema (Just j) = validateJsonAgainstSchema errorFactory o schema j

confirmProperties ∷ ∀ a. Monoid a ⇒ Eq a ⇒ (String → a) → AST.OpenAPIObject → Array String → Map.Map String (AST.ReferenceOr AST.Schema) → Map.Map String AST.JSON → a
confirmProperties errorFactory o r m j =
  fold
    $ map
        ( \(Tuple k v) →
            confirmProperty
              (eff errorFactory $ " -- in schema at property " <> k)
              o
              (not (notElem k r))
              v
              (Map.lookup k j)
        )
        (Map.toUnfoldable m ∷ List (Tuple String (AST.ReferenceOr AST.Schema)))

confirmAdditionalProperties ∷ ∀ a. Monoid a ⇒ Eq a ⇒ (String → a) → AST.OpenAPIObject → AST.Additionals → Map.Map String AST.JSON → a
confirmAdditionalProperties errorFactory o a j =
  fold
    $ map
        ( \(Tuple k v) →
            confirmAdditionalProperty
              (eff errorFactory $ " -- in schema at additionalProperty " <> k)
              o
              a
              v
        )
        (Map.toUnfoldable j ∷ List (Tuple String AST.JSON))

confirmAdditionalProperty ∷ ∀ a. Monoid a ⇒ Eq a ⇒ (String → a) → AST.OpenAPIObject → AST.Additionals → AST.JSON → a
confirmAdditionalProperty errorFactory _ (AST.AdditionalBoolean true) _ = mempty

confirmAdditionalProperty errorFactory _ (AST.AdditionalBoolean false) _ = errorFactory $ "Additional properties are disallowed."

confirmAdditionalProperty errorFactory o (AST.AdditionalReference r) j = validateJsonAgainstSchema errorFactory o (AST.Ref r) j

confirmAdditionalProperty errorFactory o (AST.AdditionalSchema s) j = validateJsonAgainstSchema errorFactory o (AST.RealDeal s) j

type ValidateArrayOfJsonAgainstArrayOfSchemaStep a
  = { acc ∷ a, ror ∷ List (AST.ReferenceOr AST.Schema), json ∷ List AST.JSON }

validateArrayOfJsonAgainstArrayOfSchema ∷
  ∀ a.
  Eq a ⇒
  Monoid a ⇒
  (String → a) →
  AST.OpenAPIObject →
  Array (AST.ReferenceOr AST.Schema) →
  Array (AST.JSON) →
  a
validateArrayOfJsonAgainstArrayOfSchema errorFactory o ror json = tailRec go { acc: mempty, ror: List.fromFoldable ror, json: List.fromFoldable json }
  where
  mismatchError = Done $ errorFactory "Array size mismatch"

  go ∷ ValidateArrayOfJsonAgainstArrayOfSchemaStep a → Step (ValidateArrayOfJsonAgainstArrayOfSchemaStep a) a
  go { acc, ror: Nil, json: Nil } = Done acc

  go { acc, ror: (x : xs), json: (y : ys) } = if List.length xs /= List.length ys then mismatchError else Loop { acc: acc <> (validateJsonAgainstSchema errorFactory o x y), ror: xs, json: ys }

  go { acc, ror: _, json: _ } = mismatchError

type AnyOfAcc a
  = { acc ∷ a, s ∷ List (AST.ReferenceOr AST.Schema), j ∷ AST.JSON }

validateString' ∷
  ∀ a.
  Monoid a ⇒
  (String → a) →
  String →
  String →
  a
validateString' errorFactory s r =
  either
    identity
    (\re → if (test re s) then mempty else errorFactory $ "Could not construct a match using regex " <> r <> " against string " <> s)
    (go (regex r noFlags))
  where
  go ∷ Either String Regex → Either a Regex
  go (Left e) = Left $ errorFactory ("Could not parse a regex using regex " <> r <> " due to error " <> e)

  go (Right g) = Right g

validateString ∷
  ∀ a.
  Monoid a ⇒
  (String → a) →
  String →
  Maybe (Array AST.JSON) →
  Maybe String →
  Maybe Int →
  Maybe Int →
  a
validateString errorFactory s (Just e) _ _ _ = if find ((==) (AST.JString s)) e == Nothing then errorFactory $ "Could not find enum " <> s <> " in list " <> show e else mempty

validateString errorFactory s Nothing r (Just minLength) maxLength = if length s < minLength then errorFactory $ "String " <> s <> "less than minimum length " <> show minLength else validateString errorFactory s Nothing r Nothing maxLength

validateString errorFactory s Nothing r minLength (Just maxLength) = if length s > maxLength then errorFactory $ "String " <> s <> "greater than maximum length " <> show maxLength else validateString errorFactory s Nothing r minLength Nothing

validateString errorFactory s Nothing (Just r) _ _ = validateString' errorFactory s r

validateString errorFactory s Nothing Nothing _ _ = mempty

validateNumber ∷
  ∀ a.
  Monoid a ⇒
  (String → a) →
  a →
  Number →
  Maybe (Array AST.JSON) →
  Maybe Number →
  Maybe Number →
  Maybe AST.BooleanInt →
  Maybe AST.BooleanInt →
  Maybe Number →
  a
validateNumber errorFactory acc n (Just e) _ _ _ _ _ =
  if find ((==) (AST.JNumber n)) e == Nothing then
    errorFactory $ "Could not find enum " <> show n <> " in list " <> show e
  else
    mempty

validateNumber errorFactory acc n Nothing (Just minimum) maximum (Just (AST.ABoolean exclusiveMinimum)) exclusiveMaximum multipleOf =
  validateNumber
    errorFactory
    (if n <= minimum then errorFactory (" n " <> show n <> " is less than its exclusive minimum " <> show minimum) <> acc else acc)
    n
    Nothing
    Nothing
    maximum
    Nothing
    exclusiveMaximum
    multipleOf

validateNumber errorFactory acc n Nothing (Just minimum) maximum exclusiveMinimum exclusiveMaximum multipleOf =
  validateNumber errorFactory
    ( if n < minimum then
        errorFactory
          ( " n " <> show n
              <> " is less than its minimum "
              <> show minimum
          )
          <> acc
      else
        acc
    )
    n
    Nothing
    Nothing
    maximum
    Nothing
    exclusiveMaximum
    multipleOf

validateNumber errorFactory acc n Nothing Nothing maximum (Just (AST.AnInt exclusiveMinimum)) exclusiveMaximum multipleOf =
  validateNumber errorFactory
    ( if n <= (Int.toNumber exclusiveMinimum) then
        errorFactory
          ( " n " <> show n <> " is less than its exclusive minimum "
              <> show exclusiveMinimum
          )
          <> acc
      else
        acc
    )
    n
    Nothing
    Nothing
    maximum
    Nothing
    exclusiveMaximum
    multipleOf

validateNumber errorFactory acc n Nothing minimum (Just maximum) exclusiveMinimum (Just (AST.ABoolean exclusiveMaximum)) multipleOf =
  validateNumber errorFactory
    ( if n >= maximum then
        errorFactory
          ( " n " <> show n
              <> " is greater than its exclusive maximum "
              <> show maximum
          )
          <> acc
      else
        acc
    )
    n
    Nothing
    minimum
    Nothing
    exclusiveMinimum
    Nothing
    multipleOf

validateNumber errorFactory acc n Nothing minimum (Just maximum) exclusiveMinimum exclusiveMaximum multipleOf =
  validateNumber errorFactory
    ( if n > maximum then
        errorFactory
          ( " n " <> show n <> " is greater than its maximum "
              <> show minimum
          )
          <> acc
      else
        acc
    )
    n
    Nothing
    minimum
    Nothing
    exclusiveMinimum
    Nothing
    multipleOf

validateNumber errorFactory acc n Nothing minimum Nothing exclusiveMinimum (Just (AST.AnInt exclusiveMaximum)) multipleOf =
  validateNumber errorFactory
    ( if n >= (Int.toNumber exclusiveMaximum) then
        errorFactory
          ( " n " <> show n <> " is greater than its exclusive maximum "
              <> show exclusiveMaximum
          )
          <> acc
      else
        acc
    )
    n
    Nothing
    minimum
    Nothing
    exclusiveMinimum
    Nothing
    multipleOf

validateNumber errorFactory acc n Nothing minimum maximum exclusiveMinimum exclusiveMaximum (Just multipleOf) =
  validateNumber errorFactory
    ( if (n / multipleOf) /= 0.0 then
        errorFactory
          ( " n " <> show n <> " is not a multiple of "
              <> show multipleOf
          )
          <> acc
      else
        acc
    )
    n
    Nothing
    minimum
    maximum
    exclusiveMinimum
    exclusiveMaximum
    Nothing

validateNumber _ acc _ _ _ _ _ _ _ = acc

validateJsonAgainstSchema ∷
  ∀ a.
  Eq a ⇒
  Monoid a ⇒
  (String → a) →
  AST.OpenAPIObject →
  AST.ReferenceOr AST.Schema →
  AST.JSON →
  a
validateJsonAgainstSchema errorFactory o ror __json =
  maybe
    (errorFactory $ "Could not match reference for " <> writeJSON ror)
    (flip go __json)
    (L.preview (referenceOrSchemaPrism o) ror)
  where
  anyOfLoop ∷ AnyOfAcc a → Step (AnyOfAcc a) a
  anyOfLoop { acc, s: Nil } = Done acc

  anyOfLoop { acc, s: (x : xs), j } =
    let
      v = validateJsonAgainstSchema errorFactory o x j
    in
      if v == mempty then
        Done mempty
      else
        Loop { acc: v <> acc, s: xs, j }

  go ∷ AST.Schema → AST.JSON → a
  go (AST.Schema skm@{ _allOf: Just a }) j =
    ( fold
        $ map
            ( \s →
                validateJsonAgainstSchema
                  (eff errorFactory " -- in allOf ")
                  o
                  s
                  j
            )
            a
    )
      <> (go (AST.Schema $ skm { _allOf = Nothing }) j)

  go (AST.Schema skm@{ _anyOf: Just a }) j =
    (tailRec anyOfLoop { acc: mempty, s: List.fromFoldable a, j })
      <> (go (AST.Schema $ skm { _anyOf = Nothing }) j)

  go (AST.Schema skm@{ _oneOf: Just a }) j =
    (tailRec anyOfLoop { acc: mempty, s: List.fromFoldable a, j })
      <> (go (AST.Schema $ skm { _oneOf = Nothing }) j)

  go (AST.Schema skm@{ _not: Just a }) j =
    ( if (validateJsonAgainstSchema errorFactory o a j) == mempty then
        errorFactory "A 'not' directive got a positive result"
      else
        mempty
    )
      <> (go (AST.Schema $ skm { _not = Nothing }) j)

  -- if properties are present, it must be an object
  go ( AST.Schema
      { _properties: Just (AST.OAIMap m)
    , _additionalProperties
    , _required
    }
  ) (AST.JObject (AST.OAIMap j)) =
    confirmProperties errorFactory o (fromMaybe [] _required) m j
      <> confirmAdditionalProperties
          errorFactory
          o
          (fromMaybe (AST.AdditionalBoolean true) _additionalProperties)
          ( Map.filterWithKey
              ( \k _ →
                  notElem
                    k
                    ( map
                        fst
                        ( Map.toUnfoldable m ∷
                            Array (Tuple String (AST.ReferenceOr AST.Schema))
                        )
                    )
              )
              j
          )

  -- fallback if properties are not present but additionals are
  go ( AST.Schema
      { _properties: Nothing
    , _additionalProperties: (Just ap)
    }
  ) (AST.JObject (AST.OAIMap j)) = confirmAdditionalProperties errorFactory o ap j

  -- fallback if type is an object but there is no other information about it
  go (AST.Schema { _type: Just "object" }) (AST.JObject (AST.OAIMap j)) = mempty

  -- type is an array with no specification about items
  go (AST.Schema { _type: Just "array", _items: Nothing }) (AST.JArray _) = mempty

  -- type is an array with tuple specification about items
  go ( AST.Schema
      { _type: Just "array"
    , _items: (Just (AST.ItemsAsTuple iat))
    }
  ) (AST.JArray a) = validateArrayOfJsonAgainstArrayOfSchema (eff errorFactory " -- in array ") o iat a

  -- type is an array with schema about items
  go ( AST.Schema
      { _type: Just "array"
    , _items: (Just (AST.SingleItem i))
    }
  ) (AST.JArray a) = fold $ map (validateJsonAgainstSchema (eff errorFactory " -- in array ") o $ AST.RealDeal i) a

  -- type is an array with reference about items
  go ( AST.Schema
      { _type: Just "array"
    , _items: (Just (AST.SingleItemReference i))
    }
  ) (AST.JArray a) =
    fold
      $ map
          ( validateJsonAgainstSchema
              (eff errorFactory " -- in array ")
              o
              $ AST.Ref i
          )
          a

  -- type is null
  go (AST.Schema { _type: Just "null" }) AST.JNull = mempty

  -- type is boolean
  go (AST.Schema { _type: Just "boolean" }) (AST.JBoolean _) = mempty

  -- type is an integer
  go ( AST.Schema
      { _type: Just "integer"
    , _enum
    , _minimum
    , _maximum
    , _exclusiveMinimum
    , _exclusiveMaximum
    , _multipleOf
    }
  ) (AST.JNumber n) =
    validateNumber
      (eff errorFactory " -- validating number ")
      mempty
      n
      _enum
      _minimum
      _maximum
      _exclusiveMinimum
      _exclusiveMaximum
      _multipleOf

  -- type is a number
  go ( AST.Schema
      { _type: Just "number"
    , _enum
    , _minimum
    , _maximum
    , _exclusiveMinimum
    , _exclusiveMaximum
    , _multipleOf
    }
  ) (AST.JNumber n) =
    validateNumber
      (eff errorFactory " -- validating number ")
      mempty
      n
      _enum
      _minimum
      _maximum
      _exclusiveMinimum
      _exclusiveMaximum
      _multipleOf

  -- type is a string
  go (AST.Schema { _type: Just "string", _pattern, _enum, _minLength, _maxLength }) (AST.JString s) =
    validateString
      (eff errorFactory " -- validating number ")
      s
      _enum
      _pattern
      _minLength
      _maxLength

  -- fallback if there is no type, which can be anything
  go (AST.Schema { _type: Nothing }) _ = mempty

  -- error catcher. this means we haven't implemented it yet
  go s j =
    errorFactory
      ( "Could not match "
          <> writeJSON j
          <> " for schema "
          <> writeJSON s
      )

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
