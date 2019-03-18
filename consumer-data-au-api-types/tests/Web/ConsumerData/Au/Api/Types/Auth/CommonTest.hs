{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web.ConsumerData.Au.Api.Types.Auth.CommonTest where

import           Control.Lens                              (makeClassyPrisms)
import           Control.Monad.Except                      (runExceptT)
import qualified Crypto.JOSE.Error                         as JE
import           Crypto.JWT.Pretty
    (AsPrettyJwtError (..), PrettyJwtError)
import           Data.Aeson
    (eitherDecode, encode)
import           Hedgehog
    (Property, failure, property, success, tripping)
import           Hedgehog.Internal.Property                (forAllT)
import           Test.Tasty                                (TestTree)
import           Test.Tasty.Hedgehog                       (testProperty)
import           Text.URI                                  (mkScheme)
import           Text.URI.Gens                             (genAuthority)
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

data GoldenError = InvalidGoldenE Error | AuthE Error | PrettyJwtE PrettyJwtError deriving (Eq, Show)

makeClassyPrisms ''GoldenError

instance AsPrettyJwtError GoldenError where
  _PrettyJwtError = _PrettyJwtE . _PrettyJwtError

instance AsError GoldenError where
  _Error = _AuthE . _Error

instance JE.AsError GoldenError where
  _Error = _Error . _JoseError

authTestPath ::
  FilePath
authTestPath =
  "tests/Web/ConsumerData/Au/Api/Types/Auth"
