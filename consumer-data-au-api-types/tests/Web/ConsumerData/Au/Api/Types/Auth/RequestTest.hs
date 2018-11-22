{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.ConsumerData.Au.Api.Types.Auth.RequestTest where

import           Control.Lens         (( # ), (^.))
import           Control.Monad        ((<=<))
import           Control.Monad.Catch  (Exception, MonadThrow, throwM)
import           Control.Monad.Except (ExceptT)
import           Crypto.JOSE
    (Alg (ES256, PS256), decodeCompact, encodeCompact)
import qualified Crypto.JOSE.JWK      as JWK
import           Crypto.JWT           (Audience (Audience), string, uri)
-- import           Data.Aeson             (encode, eitherDecode')
import Data.ByteString.Lazy (ByteString)
import Network.URI          (parseURI)

import           Hedgehog
    (MonadGen, Property, PropertyT, evalExceptT, property, (===))
import qualified Hedgehog.Gen as Gen
-- `forAllT` should probs be public: https://github.com/hedgehogqa/haskell-hedgehog/issues/203
import           Hedgehog.Internal.Property              (forAllT)
import qualified Hedgehog.Range                          as Range
import           Test.Tasty                              (TestTree)
import           Test.Tasty.Hedgehog                     (testProperty)
import           Text.URI.Gens                           (genURI)
import           Web.ConsumerData.Au.Api.Types.Auth.Gens (genJWK)
import           Web.ConsumerData.Au.Api.Types.Auth.Gens (genClaims)

import Web.ConsumerData.Au.Api.Types.Auth.AuthorisationRequest
    (AuthorisationRequest (AuthorisationRequest), authRequestToJwt,
    jwtToAuthRequest)
import Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientId (ClientId), Nonce (Nonce), RedirectUri (RedirectUri),
    ResponseType (CodeIdToken), Scope (..), mkScopes)
import Web.ConsumerData.Au.Api.Types.Auth.Error                (Error)

test_request ::
  [TestTree]
test_request =
  [ testProperty "round trip auth request" roundTripAuthRequest
  ]

roundTripAuthRequest ::
  Property
roundTripAuthRequest =
  property $ do
    ar <- forAllT genAuthRequest
    jwk <- forAllT genJWK
    let
      alg = signingAlg (jwk ^. JWK.jwkMaterial)

      mkJwt :: AuthorisationRequest -> ExceptT Error (PropertyT IO) ByteString
      mkJwt = fmap encodeCompact . authRequestToJwt jwk alg

      mkAr =  jwtToAuthRequest (const True) (const True) jwk <=< decodeCompact

    (=== ar) <=< evalExceptT . (mkAr <=< mkJwt) $ ar

data BadAudParse =
  BadAudParse
  deriving (Show)
instance Exception BadAudParse

genAuthRequest ::
  forall n.
  ( MonadGen n
  , MonadThrow n
  )
  => n AuthorisationRequest
genAuthRequest =
  let
    genUnicodeText a b =
      Gen.text (Range.linear a b) Gen.unicode
    genAdditionalScopes =
      Gen.set (Range.linear 0 2) (Gen.element [ProfileScope, EmailScope])
    mUri = parseURI "https://lambdabank.io"
    throwParse :: n a
    throwParse = throwM BadAudParse
    genAud =
      Audience <$> traverse (maybe throwParse (pure . (uri #))) [mUri]
  in
    AuthorisationRequest
    <$> pure CodeIdToken
    <*> (ClientId <$> genUnicodeText 1 15)
    <*> (RedirectUri <$> genURI)
    <*> (mkScopes <$> genAdditionalScopes)
    <*> pure Nothing
    <*> (Nonce <$> genUnicodeText 10 10)
    <*> pure (string # "qfpl.io")
    <*> genAud
    <*> genClaims


-- | Valid signing algorithms are specified in
-- <https://openid.net/specs/openid-financial-api-part-2.html#jws-algorithm-considerations FAPI R+W §8.6>.
-- These choices dictate the key material that is valid --- see 'genKeyMaterial'
signingAlg ::
  JWK.KeyMaterial
  -> Alg
signingAlg = \case
  JWK.ECKeyMaterial _ -> ES256
  _ -> PS256

