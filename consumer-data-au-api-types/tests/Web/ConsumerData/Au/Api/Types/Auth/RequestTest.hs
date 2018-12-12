{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.ConsumerData.Au.Api.Types.Auth.RequestTest where

import           Control.Exception      (throw)
import           Control.Lens           (( # ))
import           Control.Monad          ((<=<))
import           Control.Monad.Catch    (Exception, MonadThrow, throwM)
import           Control.Monad.Except
    (ExceptT (ExceptT), MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Crypto.JOSE
    (Alg (ES256), decodeCompact, encodeCompact)
import qualified Crypto.JOSE.JWK        as JWK
import           Crypto.JWT
    (Audience (Audience), defaultJWTValidationSettings, uri, verifyClaims)
import           Data.Aeson             (eitherDecode')
import           Data.Bifunctor         (first)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Dependent.Map     as DM
import           Data.Dependent.Sum     (DSum ((:=>)))
import           Data.Maybe             (fromJust)
import qualified Data.Set               as Set
import           Network.URI            (parseURI)
import           Text.URI               (mkURI)

import           Hedgehog     (MonadGen, Property, evalExceptT, property, (===))
import qualified Hedgehog.Gen as Gen
-- `forAllT` should probs be public: https://github.com/hedgehogqa/haskell-hedgehog/issues/203
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range             as Range
import           Test.Tasty                 (TestTree)
import           Test.Tasty.Hedgehog        (testProperty)

import AesonGolden (aesonGolden)
import Text.URI.Gens                           (genURI)
import Web.ConsumerData.Au.Api.Types.Auth.Gens (genClaims,genJWK)

import Web.ConsumerData.Au.Api.Types.Auth.AuthorisationRequest
    (AuthorisationRequest (AuthorisationRequest), Claims (Claims),
    authRequestToJwt, jwtToAuthRequest)
import Web.ConsumerData.Au.Api.Types.Auth.Common
    (Acr (Acr), Claim (Claim), ClientId (ClientId), IdToken (IdToken),
    IdTokenClaims, IdTokenKey (IdTokenSub), Nonce (Nonce),
    Prompt (SelectAccount), RedirectUri (RedirectUri),
    ResponseType (CodeIdToken), Scope (..), State (..), TokenSubject (..),
    mkScopes)
import Web.ConsumerData.Au.Api.Types.Auth.Error
    (Error (ParseError))

test_request ::
  [TestTree]
test_request =
  [ testProperty "round trip auth request" roundTripAuthRequest
  , golden
  ]

roundTripAuthRequest ::
  Property
roundTripAuthRequest =
  property $ do
    ar <- forAllT genAuthRequest
    (jwk,alg) <- forAllT genJWK
    let
      mkAr =  jwtToAuthRequest (const True) (const True) jwk <=< decodeCompact

    (=== ar) <=< evalExceptT . (mkAr <=< mkJwt jwk alg) $ ar

golden ::
  TestTree
golden =
  let
    name = "Golden AuthorisationRequest JWT"
    gf = authTestPath <> "/compact-authorisation-request.golden"
    keyFile = authTestPath <> "/jwk.json"
    alg = ES256
    jwtValidationSettings = defaultJWTValidationSettings (const True)
    mClaims = do
      jwk <- ExceptT . fmap (first ParseError . eitherDecode') . BS.readFile $ keyFile
      jwt <- authRequestToJwt jwk alg goldenAuthRequest
      verifyClaims  jwtValidationSettings jwk jwt
    ioClaims = (>>= either (throw . JwtFailure . show) pure) . runExceptT $ mClaims
    -- The signing process uses random inputs, so just verify the header and payload hasn't changed.
    -- Round tripping ensures that the signatures are valid.
    -- ioHeaderPayload = BS.intercalate "." . take 2 . BS.split dot <$> ioJwt
  in
    aesonGolden name gf ioClaims

authTestPath ::
  FilePath
authTestPath =
  "tests/Web/ConsumerData/Au/Api/Types/Auth"

newtype JwtFailure =
  JwtFailure String
  deriving (Eq, Show)

instance Exception JwtFailure

mkJwt ::
  ( MonadIO m
  , JWK.MonadRandom m
  , MonadError Error m
  )
  => JWK.JWK
  -> Alg
  -> AuthorisationRequest
  -> m ByteString
mkJwt jwk alg =
  fmap encodeCompact . authRequestToJwt jwk alg

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
      Gen.set (Range.linear 0 2) (Gen.element [ProfileScope])
    mUri = parseURI "https://lambdabank.io"
    throwParse :: n a
    throwParse = throwM BadAudParse
    genAud =
      Audience <$> traverse (maybe throwParse (pure . (uri #))) [mUri]
  in
    AuthorisationRequest
    <$> pure CodeIdToken
    <*> (ClientId <$> genUnicodeText 1 15)
    <*> (RedirectUri <$> genUri)
    <*> (mkScopes <$> genAdditionalScopes)
    <*> (State <$> genUnicodeText 10 10)
    <*> (Nonce <$> genUnicodeText 10 10)
    <*> pure SelectAccount
    <*> genAud
    <*> genClaims

goldenAuthRequest ::
  AuthorisationRequest
goldenAuthRequest =
  AuthorisationRequest
    CodeIdToken
    (ClientId "functionalfinance.io")
    (RedirectUri . fromJust . mkURI $ "https://functionalfinance.io/auth")
    (mkScopes Set.empty)
    (State "r4nd0m")
    (Nonce "fhqwgad")
    SelectAccount
    (Audience [uri # fromJust (parseURI "https://lambdabank.io")])
    (Claims Nothing goldenIdToken)

goldenIdToken ::
  IdTokenClaims
goldenIdToken =
  let
    acrClaim = Claim [Acr "urn:cds.au:cdr:3"] True
    claimsMap = DM.fromList
      [ IdTokenSub :=> Claim [TokenSubject "u12345"] False
      ]
  in
    IdToken Nothing Nothing acrClaim claimsMap
