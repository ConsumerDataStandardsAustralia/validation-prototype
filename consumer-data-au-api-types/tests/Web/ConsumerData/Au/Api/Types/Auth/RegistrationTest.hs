{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.ConsumerData.Au.Api.Types.Auth.RegistrationTest where

import           Control.Exception                        (IOException)
import           Control.Lens                             (( # ))
import           Control.Monad.Catch
    (Exception, MonadThrow, throwM)
import           Crypto.JWT
    (Audience (Audience), NumericDate (..), StringOrURI, string, uri)
import           Data.Aeson
    (FromJSON (..), Result (..), ToJSON (..), Value, fromJSON)
import           Data.Maybe                               (isNothing)
import qualified Data.Set                                 as Set
import           Data.Time.Calendar                       (fromGregorian)
import           Data.Time.Clock
import           Hedgehog
    (MonadGen, Property, assert, property, (===))
import qualified Hedgehog.Gen                             as Gen
import           Network.URI                              (parseURI)
import           Text.URI
    (Authority (Authority), RText, RTextLabel (Scheme), URI (URI), mkHost,
    mkScheme, renderStr)
import           Text.URI.Gens
    (genAuthority, genPathPieces, genScheme, genURI)
import           Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsError, _MissingClaim, _ParseError)
-- `forAllT` should probs be public: https://github.com/hedgehogqa/haskell-hedgehog/issues/203
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range             as Range
import           Test.Tasty                 (TestTree)
import           Test.Tasty.Hedgehog        (testProperty)

import Web.ConsumerData.Au.Api.Types.Auth.Common
    (FapiPermittedAlg (..), RedirectUri (RedirectUri), ResponseType (..),
    Scope (..), mkScopes)
import Web.ConsumerData.Au.Api.Types.Auth.Registration

test_request ::
  [TestTree]
test_request =
  [
  testProperty "The 'redirect_urls' smart constructor only accepts https  && !localhost hosts." redirectUrlsValid
  , testProperty "The 'redirect_urls' smart constructor rejects any non-https or localhost hosts." redirectUrlsInvalid
  , testProperty "Redirect request round-trips to/from JWT." regoRoundTrips
  ]

redirectUrlsValid ::
  Property
redirectUrlsValid =
  property $ do
    uris <-getRedirectUrls <$> forAllT genRedirectUrls
    assert (not.null$uris)

redirectUrlsInvalid ::
  Property
redirectUrlsInvalid =
  property $ do
    mRedirectUrl<- redirectUrls <$> forAllT genInvalidRedirectUris
    assert (isNothing mRedirectUrl)

regoRoundTrips::
  Property
regoRoundTrips =
  property $ do
    regoReq <- forAllT genRegReq
    regoReq' <- fromVal.toJSON $ regoReq
    regoReq === regoReq'

--TODO:
instance AsError IOException where

genRegReq::
  ( MonadGen n
  , MonadThrow n
  )
  => n RegistrationRequest
genRegReq =
  RegistrationRequest <$> genJwtHeaders <*> genMeta <*> (GenerateSs <$> genSs)

genJwtHeaders::
  ( MonadGen n
  , MonadThrow n
  )
  => n JwsSigningHeaders
genJwtHeaders =
  JwsSigningHeaders <$> (Just <$> genStringOrUri) <*> (Just <$> genAud) <*> (Just <$> genNumDate) <*> (Just <$> genNumDate) <*> (Just <$> genJti)

genStringOrUri::
  ( MonadGen n
  , MonadThrow n
  )
  => n StringOrURI
genStringOrUri = Gen.choice [(uri #) <$> uri', (string #) <$> str]
  where uri'= m2e BadUri =<< parseURI.renderStr <$> genURI
        str = Gen.string (Range.linear 10 10) Gen.unicode

m2e :: forall m e a.
     (MonadThrow m, Exception e) =>
     e -> Maybe a -> m a
m2e e = maybe (throwM e) pure

-- Might actually want a relevant time here
genNumDate ::
  ( MonadGen n
  , MonadThrow n
  )
  => n NumericDate
genNumDate = NumericDate . utc <$> Gen.integral (Range.linear 1 31)
  where utc a = UTCTime (fromGregorian 2018 1 a) (secondsToDiffTime 1)

--TODO: check if multi `aud` URLs are allowed.
genAud::
  ( MonadGen n
  , MonadThrow n
  )
  => n Audience
genAud = Audience <$> Gen.list (Range.linear 10 10) genStringOrUri

genJti ::
  ( MonadGen n
  , MonadThrow n
  )
  => n JTI
genJti  = JTI <$> Gen.text (Range.linear 10 10) Gen.unicode

genMeta ::
  ( MonadGen n
  , MonadThrow n
  )
  => n ClientMetaData
genMeta =
  ClientMetaData <$>
    genAlg <*>
    genGrantTypes <*>
    genApplicationType <*>
    genAuthMeth <*>
    Gen.maybe genScript <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genContacts <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genSubjectType<*>
    Gen.maybe genUrlHttps <*>
    Gen.maybe genJwks <*>
    Gen.maybe genRequestUris <*>
    genRedirectUrls <*>
    Gen.maybe (RequestObjectEncryption <$> genAlg <*> Gen.maybe genEnc)     <*>
    Gen.maybe genAlg <*>
    Gen.maybe (IdTokenEncryption <$> genAlg <*> Gen.maybe genEnc)     <*>
    Gen.maybe genResponseTypes <*>
    Gen.maybe (DefaultMaxAge <$> Gen.int (Range.linear 1 10000)) <*>
    Gen.maybe Gen.bool <*>
    Gen.maybe genAcr <*>
    Gen.maybe genUrlHttps <*>
    Gen.maybe (UserInfoEncryption <$> genAlg <*> Gen.maybe genEnc)     <*>
    genAlg <*>
    Gen.maybe genScopes <*>
    Gen.maybe (SoftwareId <$> Gen.text (Range.linear 10 10) Gen.unicode) <*>
    Gen.maybe (SoftwareVersion <$> Gen.text (Range.linear 10 10) Gen.unicode)

genSs::
  ( MonadGen n
  , MonadThrow n
  )
  => n SoftwareStatement
genSs = SoftwareStatement <$> genJwtHeaders <*> genMeta

genAlg :: ( MonadGen n , MonadThrow n ) => n FapiPermittedAlg
genAlg = Gen.element [PS256,ES256]

genGrantTypes :: ( MonadGen n , MonadThrow n ) => n FapiGrantTypes
genGrantTypes = m2e BadGrantType $ fapiGrantTypes . GrantTypes . Set.fromList $ [AuthorizationCode]

genApplicationType :: ( MonadGen n , MonadThrow n ) => n FapiApplicationType
genApplicationType = m2e BadApplicationType =<< fapiApplicationType <$> Gen.element [Web]

genAuthMeth :: ( MonadGen n , MonadThrow n ) => n FapiTokenEndpointAuthMethod
genAuthMeth = fapiTokenEndpointAuthMethod <$> (Gen.element [PrivateKeyJwt,ClientSecretJwt] <*> genAlg) >>= maybe (throwM BadAuthMeth) pure

genScript :: ( MonadGen n , MonadThrow n ) => n Script
genScript = Script DefaultLang <$> Gen.text (Range.linear 10 10) Gen.unicode

genScriptUri :: ( MonadGen n , MonadThrow n ) => n ScriptUri
genScriptUri = ScriptUri DefaultLang <$> genURI

genContacts :: ( MonadGen n , MonadThrow n ) => n RegistrationContacts
genContacts = RegistrationContacts <$> Gen.list (Range.linear 10 10) (EmailAddress <$> Gen.text (Range.linear 10 10) Gen.unicode)

genSubjectType :: ( MonadGen n , MonadThrow n ) => n SubjectType
genSubjectType = Gen.element [Pairwise, Public]

genJwks :: ( MonadGen n , MonadThrow n ) => n JwkSet
genJwks = Gen.choice [JwksRef . JwksUri <$> genURI , JwksVal <$> Gen.text (Range.linear 10 10) Gen.unicode]

genRequestUris :: ( MonadGen n , MonadThrow n ) => n RequestUris
genRequestUris = RequestUris <$> (map RequestUri <$> Gen.list (Range.linear 10 10) genURI)

genEnc :: ( MonadGen n , MonadThrow n ) => n FapiEnc
genEnc = Gen.element [A128CBC_HS256 , A192CBC_HS384 , A256CBC_HS512 , A128GCM , A192GCM , A256GCM]

genResponseTypes :: ( MonadGen n , MonadThrow n ) => n FapiResponseTypes
genResponseTypes = FapiResponseTypes <$> genSubs [CodeIdToken, CodeIdTokenToken]

genSubs  :: ( MonadGen n , MonadThrow n ) => [a] -> n [a]
genSubs as = Gen.shuffle as >>= Gen.subsequence

genAcr :: ( MonadGen n , MonadThrow n ) => n FapiAcrValues
genAcr = FapiAcrValues <$> Gen.text (Range.linear 10 10) Gen.unicode

genScopes :: ( MonadGen n , MonadThrow n ) => n FapiScopes
genScopes = FapiScopes . mkScopes . Set.fromList <$> genSubs  [ ProfileScope, EmailScope]

genUrlHttps::
  ( MonadGen n
  , MonadThrow n
  )
  => n HttpsUrl
genUrlHttps = do
  https <- mkScheme "https"
  autho <- genAuthority
  HttpsUrl <$> genUrl https autho

genRedirectUrls::
  ( MonadGen n
  , MonadThrow n
  )
  => n RedirectUrls
genRedirectUrls = do
  https <- mkScheme "https"
  lhAutho <- (\l -> Authority Nothing l Nothing) <$> mkHost "localhost"
  autho <-Gen.filter (/=lhAutho) genAuthority
  mRedirectUris <- redirectUrls . map RedirectUri <$> genUrls https autho
  maybe (throwM BadRedirectUri) pure mRedirectUris

genInvalidRedirectUris::
  ( MonadGen n
  , MonadThrow n
  )
  => n [RedirectUri]
genInvalidRedirectUris = do
  let
    goodScheme = mkScheme "https"
    badAutho = (\l -> Authority Nothing l Nothing) <$> mkHost "localhost"
    scheme = Gen.choice [goodScheme,genScheme]
    autho = Gen.choice [genAuthority,badAutho]
    --filter for those that are https and !localhost (i.e both valid)
    bothvalidf gs ba = Gen.filter (\(s,a) ->not (s == gs && a /= ba))
  gs <- goodScheme
  ba <- badAutho
  (bscheme,bautho) <- bothvalidf gs ba $ (,) <$> scheme <*> autho
  map RedirectUri <$> genUrls bscheme bautho

data BadRedirectUri = BadRedirectUri deriving (Show)
instance Exception BadRedirectUri

data BadUri = BadUri deriving (Show)
instance Exception BadUri

data BadGrantType = BadGrantType deriving (Show)
instance Exception BadGrantType

data BadApplicationType = BadApplicationType  deriving (Show)
instance Exception BadApplicationType

data BadAuthMeth = BadAuthMeth  deriving (Show)
instance Exception BadAuthMeth

genUrls ::
  ( MonadGen n
  , MonadThrow n
  )
  => RText 'Scheme -> Authority -> n [URI]
genUrls scheme auth = Gen.list (Range.linear 1 10) $ genUrl scheme auth

genUrl ::
  ( MonadGen n
  , MonadThrow n
  )
  => RText 'Scheme -> Authority -> n URI
genUrl scheme auth = URI
    <$> pure (Just scheme)
    <*> pure (Right auth)
    <*> genPathPieces
    <*> pure []
    <*> pure Nothing
