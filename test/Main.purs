module Test.Main where

import Prelude
import Data.Foldable (fold)
import Data.Array (replicate)
import Data.Either (either)
import Data.HttpTypes.V000 (Header(..), HttpTypesMap(..), Method(..), Query(..), Request(..))
import Data.List.NonEmpty (length)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.OpenAPI.V300 (OpenAPIObject)
import Data.OpenAPI.V300.Validator (getViablePathItemsForRequest)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff as Effect
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (readJSON)
import Test.Spec (before, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

readOpenAPI :: String -> Effect.Aff OpenAPIObject
readOpenAPI t = do
  rtf <- liftEffect $ readTextFile UTF8 t
  either (liftEffect <<< throw <<< const "Could not parse openapi") pure $ readJSON rtf

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "stripe" do
          before (readOpenAPI "oai/stripe.json") do
            it "should succeed on GET /v1/account/capabilities" \oai -> do
              let
                req =
                  Request
                    { method: GET
                    , path: Just "/v1/account/capabilities"
                    , pathname: Just "/v1/account/capabilities"
                    , body: Nothing
                    , host: Nothing
                    , headers: Nothing
                    , url: Nothing
                    , query: Nothing
                    , timestamp: Nothing
                    , protocol: Nothing
                    }
              either (fail <<< show) (const $ pure unit) (getViablePathItemsForRequest oai req)
            it "should fail on GET /v1/account/capabilitiees" \oai -> do
              let
                req =
                  Request
                    { method: GET
                    , path: Just "/v1/account/capabilitiees"
                    , pathname: Just "/v1/account/capabilitiees"
                    , body: Nothing
                    , host: Nothing
                    , headers: Nothing
                    , url: Nothing
                    , query: Nothing
                    , timestamp: Nothing
                    , protocol: Nothing
                    }
              either (const $ pure unit) (fail <<< show <<< length) (getViablePathItemsForRequest oai req)
            it "should succeed on GET /v1/3d_secure/{three_d_secure}" \oai -> do
              let
                req =
                  Request
                    { method: GET
                    , path: Just "/v1/3d_secure/foobar"
                    , pathname: Just "/v1/3d_secure/foobar"
                    , body: Nothing
                    , host: Nothing
                    , headers: Nothing
                    , url: Nothing
                    , query: Nothing
                    , timestamp: Nothing
                    , protocol: Nothing
                    }
              either (fail <<< show) (const $ pure unit) (getViablePathItemsForRequest oai req)
            it "should succeed on GET /v1/3d_secure/{three_d_secure} when extraneous queries and headers are present" \oai -> do
              let
                req =
                  Request
                    { method: GET
                    , path: Just "/v1/3d_secure/foobar"
                    , pathname: Just "/v1/3d_secure/foobar"
                    , body: Nothing
                    , host: Nothing
                    , headers: Just (HttpTypesMap $ Map.singleton "Foo" (StringHeader "Bar"))
                    , url: Nothing
                    , query: Just (HttpTypesMap $ Map.singleton "foo" (StringQuery "bar"))
                    , timestamp: Nothing
                    , protocol: Nothing
                    }
              either (fail <<< show) (const $ pure unit) (getViablePathItemsForRequest oai req)
            it "should fail on GET /v1/3d_secure/{three_d_secure} when max length is exceeded" \oai -> do
              let
                req =
                  Request
                    { method: GET
                    , path: Just $ "/v1/3d_secure/" <> (fold $ replicate 5001 "a")
                    , pathname: Just $ "/v1/3d_secure/" <> (fold $ replicate 5001 "a")
                    , body: Nothing
                    , host: Nothing
                    , headers: Nothing
                    , url: Nothing
                    , query: Nothing
                    , timestamp: Nothing
                    , protocol: Nothing
                    }
              either (const $ pure unit) (fail <<< show <<< length) (getViablePathItemsForRequest oai req)
