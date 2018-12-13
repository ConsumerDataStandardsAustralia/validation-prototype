{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}

module Web.ConsumerData.Au.Api.Types.Auth.CommonTest where

import           Control.Lens                              (( # ), (^?))
import           Control.Monad                             ((<=<))
import           Control.Monad.Catch
    (Exception, MonadThrow, throwM)
import           Control.Monad.Except
    (ExceptT, MonadIO, liftIO, runExceptT)
import           Crypto.JWT
    (Alg (..), Audience (Audience), NumericDate (..), StringOrURI,
    decodeCompact, encodeCompact, string, uri)
import           Data.Aeson
    (eitherDecode, encode)
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Lazy                      as LBS
import           Data.Maybe                                (isNothing)
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text)
import           Data.Time.Calendar                        (fromGregorian)
import           Data.Time.Clock
import           Hedgehog
    (MonadGen, Property, PropertyT, assert, evalEither, evalExceptT, failure,
    property, success, tripping, (===))
import qualified Hedgehog.Gen                              as Gen
import           Hedgehog.Helpers                          (sampleT)
import           Hedgehog.Internal.Property                (forAllT)
import qualified Hedgehog.Range                            as Range
import           Network.URI                               (parseURI)
import           Test.Tasty                                (TestTree)
import           Test.Tasty.Hedgehog                       (testProperty)
import           Text.URI
    (Authority (Authority), mkHost, mkScheme, renderStr)
import           Text.URI.Gens
    (genAuthority, genHost, genScheme, genUri)
import           Web.ConsumerData.Au.Api.Types.Auth.Common
import           Web.ConsumerData.Au.Api.Types.Auth.Error
import           Web.ConsumerData.Au.Api.Types.Auth.Gens

test_request ::
  [TestTree]
test_request =
  [
      testProperty "HttpsUrl JSON round-trips." httpsUrlRoundtrips
    , testProperty "Invalid HttpsUrl's cannot be constructed fromJSON." httpsUrlInvalid
  ]

httpsUrlRoundtrips ::
  Property
httpsUrlRoundtrips  =
  property $ do
    h <- forAllT genHttpsUrl
    tripping h encode eitherDecode

httpsUrlInvalid ::
  Property
httpsUrlInvalid =
  property $ do
    autho <- forAllT genAuthority
    http <- mkScheme "http"
    uri <- forAllT $ genUrl http autho
    h :: Either HttpsUrlError HttpsUrl <- runExceptT $ mkHttpsUrl uri
    either (const success) (const failure) h
