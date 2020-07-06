module Data.OpenAPI.V300.Validator.Response where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HttpTypes.V000 (HttpTypesMap)
import Data.List.Lazy (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)

newtype ResponseError
  = ResponseError
  { statusCode :: Maybe (NonEmpty List String)
  , body :: Maybe (NonEmpty List String)
  , headers :: Maybe (HttpTypesMap (NonEmpty List String))
  , timestamp :: Maybe (NonEmpty List String)
  }

derive instance newtypeResponseError :: Newtype ResponseError _

derive instance genericResponseError :: Generic ResponseError _

derive newtype instance eqResponseError :: Eq ResponseError

instance showResponseError :: Show ResponseError where
  show = genericShow
