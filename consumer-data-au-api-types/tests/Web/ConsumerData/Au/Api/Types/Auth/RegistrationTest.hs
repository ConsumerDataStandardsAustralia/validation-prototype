{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}

module Web.ConsumerData.Au.Api.Types.Auth.RegistrationTest where

import           AesonGolden                              (aesonGolden)
import           Control.Exception                        (throw)
import           Control.Lens                             (( # ), (^?))
import           Control.Monad                            ((<=<))
import           Control.Monad.Catch
    (Exception, MonadThrow, throwM)
import qualified Crypto.JOSE.JWA.JWE                      as JWE
import qualified Crypto.JOSE.JWK                          as JWK
import           Crypto.JWT
    (Alg (..), Audience (Audience), NumericDate (..), StringOrURI,
    decodeCompact, encodeCompact, string, uri)
import           Data.Aeson                               (eitherDecode')
import           Data.Bifunctor                           (first)
import           Data.ByteString                          (ByteString)
import qualified Data.ByteString.Lazy                     as LBS
import           Data.Maybe                               (isNothing)
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import           Data.Text.IO                             as Text
import           Data.Time.Calendar                       (fromGregorian)
import           Data.Time.Clock
import           Hedgehog
    (MonadGen, Property, PropertyT, assert, evalExceptT, property, (===))
import qualified Hedgehog.Gen                             as Gen
import           Network.URI                              as NetworkUri
import           Network.URI                              (parseURI)
import           Prelude                                  hiding (exp)
import           Text.URI                                 as TextUri
import           Text.URI
    (Authority (Authority), mkHost, mkScheme, mkURI, renderStr)
import           Text.URI.Gens
    (genAuthority, genScheme, genUri)
import           Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsError (..), Error, GoldenError (..), JwtFailure (..))
-- `forAllT` should probs be public: https://github.com/hedgehogqa/haskell-hedgehog/issues/203
import           Control.Monad.Except
    (ExceptT (..), MonadIO, liftIO, runExceptT)
import           Crypto.JWT.Pretty
    (JwtPart (Signature), PrettyJwt, mkPrettyJwt, removePart)
import           Hedgehog.Internal.Property                      (forAllT)
import qualified Hedgehog.Range                                  as Range
import           Test.Tasty                                      (TestTree)
import           Test.Tasty.Hedgehog                             (testProperty)
import           Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientIss (..), FapiPermittedAlg (..), RedirectUri (RedirectUri),
    ResponseType (..), Scope (..), authTestPath, mkScopes, _FapiPermittedAlg,
    _HttpsUrl)
import           Web.ConsumerData.Au.Api.Types.Auth.Gens
import           Web.ConsumerData.Au.Api.Types.Auth.Registration

test_request ::
  [TestTree]
test_request =
  [
  testProperty "The 'redirect_urls' smart constructor only accepts https  && !localhost hosts." redirectUrlsValid
  , testProperty "The 'redirect_urls' smart constructor rejects any non-https or localhost hosts." redirectUrlsInvalid
  , testProperty "Claims round-trips to/from ClaimsMap." claimsRoundTrips
  , testProperty "Redirect request round-trips to/from JWT." regoJwtRoundTrips
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
    mRedirectUrl <- (^? _RedirectUrls) <$> forAllT genInvalidRedirectUris
    assert (isNothing mRedirectUrl)

--todo tripping
claimsRoundTrips ::
  Property
claimsRoundTrips =
  property $ do
    md <- forAllT genMeta
    md' <- evalExceptT (aesonClaimsToMetaData $ metaDataToAesonClaims md :: ExceptT Error (PropertyT IO) ClientMetaData)
    md === md'

regoJwtRoundTrips::
  Property
regoJwtRoundTrips =
  property $ do
    (j,h,c) <- forAllT genRegParams
    r <- forAllT genRegReq
    let
      ar2jwt :: RegistrationRequest -> ExceptT Error (PropertyT IO) LBS.ByteString
      ar2jwt = fmap encodeCompact . regoReqToJwt j h c
      jwt2ar = jwtToRegoReq (const True) (const True) (const True) (const True) j
                 <=< decodeCompact
    (=== r) <=< evalExceptT . (fmap (\(_,_,r')->r') . jwt2ar <=< ar2jwt) $ r

genRegParams ::
  ( MonadGen n
  , MonadThrow n
  , MonadIO n
  )
  => n (JWK.JWK,JwsHeaders,JwsRegisteredClaims)
genRegParams  = do
    (j,alg) <- genJWK
    a :: FapiPermittedAlg <- Gen.just $ pure (alg ^? _FapiPermittedAlg)
    h <- JwsHeaders a <$> genKid
    c <- genRegClaims
    return (j,h,c)

golden ::
  TestTree
golden =
  let
    name = "Golden RegoRequest"
    gf = authTestPath <> "/compact-registration-request.golden"
    keyFile = authTestPath <> "/jwk.json"
    kid = FapiKid "jwt-kid"
    struri a = (fmap (uri #) . tUri2nUri =<< mkURI a) ?? JwtFailure "Invalid URI."
    mJwt :: ExceptT GoldenError IO PrettyJwt
    mJwt = do
      (i,e) <- liftIO $ genIatExp' 100
      u <- struri "https://iss.com"
      a <- struri "https://aud.com"
      let c = JwsRegisteredClaims {_iss = Just (ClientIss u), _aud = Just (Audience [a]),
        _iat = Just i, _exp = Just e, _jti = Just $ JTI "jti"}
      jwk <- ExceptT . fmap (first (_ParseError #) . eitherDecode') . LBS.readFile $ keyFile
      falg <- (ES256 ^? _FapiPermittedAlg) ?? (JwtFailure "Invalid alg.")
      jwt <- regoReqToJwt jwk (JwsHeaders falg kid) c =<< goldenRegoRequest
      removePart Signature <$> mkPrettyJwt jwt
    ioPrettyJwt :: IO PrettyJwt
    ioPrettyJwt =
      either (throw . JwtFailure . show) pure =<< runExceptT mJwt
  in
    aesonGolden name gf ioPrettyJwt

goldenRegoRequest :: (MonadThrow m,MonadIO m)
  =>  m RegistrationRequest
goldenRegoRequest = do
  let u f = mkURI f ?? JwtFailure "Invalid URI."
      s = ((?? JwtFailure "Invalid URI.") . (^? _HttpsUrl) =<<) . u
      r = ((?? JwtFailure "Invalid URI.") . ((^? _RedirectUrls). Set.singleton . RedirectUri) =<<) . u
      ssafile = authTestPath <> "/ssa.b64"
  ssa<- liftIO $ Text.readFile ssafile
  alg<- PS256 ^? _FapiPermittedAlg ?? (JwtFailure "Invalid alg.")
  enc <- JWE.A128CBC_HS256 ^? _FapiEnc ?? (JwtFailure "Invalid enc.")
  at<- Web ^? _FapiApplicationType ?? (JwtFailure "Invalid AT.")
  te<- (PrivateKeyJwt alg) ^? _FapiTokenEndpointAuthMethod ?? (JwtFailure "Invalid TEP.")
  gt<- (GrantTypes (Set.fromList [Implicit,AuthorizationCode,RefreshToken]))
      ^? _FapiGrantTypes ?? (JwtFailure "Invalid GT.")
  lu <- u "https://logo-uri.com"
  pu <- u "https://policy-uri.com"
  cu <- u "https://client-uri.com"
  tu <- u "https://terms-uri.com"
  si <- s "https://si-uri.com"
  ru <- u "https://request-uri.com"
  rdu <- r "https://redirect-uri.com"
  liu <- s "https://login-uri.com"
  nu <- s "https://notification-uri.com"
  let _requestObjectSigningAlg = alg
      _applicationType = Just at
      _tokenEndpointAuthMethod = te
      _grantTypes = Just gt
      _clientName = Just $ Script DefaultLang "client name"
      _clientUri = Just $ ScriptUri DefaultLang cu
      _contacts = Just (RegistrationContacts (Set.fromList
                           [EmailAddress {fromEmailAddress = "email@contact"}]))
      _logoUri =Just $ ScriptUri DefaultLang lu
      _policyUri =Just $ ScriptUri DefaultLang pu
      _tosUri = Just $ ScriptUri DefaultLang tu
      _subjectType = Just Pairwise
      _sectorIdentifierUri =Just si
      _keySet = Just (JwksVal "jwks_val")
      _requestUris = Just $ RequestUris {getRequestUris =
         Set.fromList [RequestUri ru]}
      _redirectUris = rdu
      _requestObjectEncryption    = Just $ RequestObjectEncryption alg (Just enc)
      _userinfoSignedResponseAlg  = Just $ alg
      _idTokenEncryption          = Just $ IdTokenEncryption alg (Just enc)
      _responseTypes              = Just $ FapiResponseTypes CodeIdToken
      _defaultMaxAge              = Just $ DefaultMaxAge 10000
      _requireAuthTime            = Just True
      _defaultAcrValues           = Just $ FapiAcrValues "@urn:cds.au:cdr:3@"
      _initiateLoginUri           = Just $ liu
      _userInfoEncryption         = Just $ UserInfoEncryption alg (Just enc)
      _idTokenSignedResponseAlg   = alg
      _scope                      = Just $
          FapiScopes (mkScopes (Set.fromList [OpenIdScope,ProfileScope]))
      _softwareId                 = Just $ SoftwareId "id1"
      _softwareVersion            = Just $ SoftwareVersion "software-version"
      _clientNotificationEndpoint = Just $ NotificationEndpoint $ nu
  pure $ RegistrationRequest (ClientMetaData {..}) (EncodedSs ssa)

genRegReq::
  ( MonadGen n
  , MonadThrow n
  , MonadIO n
  )
  => n RegistrationRequest
genRegReq =
  RegistrationRequest <$> genMeta <*> (DecodedSs <$> genSs)

genHeaders::
  ( MonadGen n
  , MonadThrow n
  , MonadIO n
  )
  => n JwsHeaders
genHeaders =  JwsHeaders <$> genAlg <*> genKid

genRegClaims::
  ( MonadGen n
  , MonadThrow n
  , MonadIO n
  )
  => n JwsRegisteredClaims
genRegClaims = do
  (i,exp) <- genIatExp
  JwsRegisteredClaims <$> (Just . ClientIss <$> genStringOrUri) <*> (Just <$> genAud) <*> pure (Just i) <*> pure (Just exp) <*> (Just <$> genJti)

genStringOrUri::
  ( MonadGen n
  , MonadThrow n
  )
  => n StringOrURI
genStringOrUri = Gen.choice [(uri #) <$> uri', (string #) <$> genText]
  where uri'= (?? BadUri) =<< tUri2nUri <$> genUri

tUri2nUri :: TextUri.URI -> Maybe NetworkUri.URI
tUri2nUri = parseURI.renderStr

-- Generate an iat and exp, with exp being + 1s to 10m later + 1 day
genIatExp ::
  ( MonadGen n
  , MonadIO n
  )
  => n (NumericDate,NumericDate)
genIatExp = genIatExp' =<< Gen.integral (Range.linear 1 600)

genIatExp' ::
  ( MonadIO n)
  => Integer -> n (NumericDate,NumericDate)
genIatExp' n = do
  iat <- liftIO getCurrentTime
  return (NumericDate iat, NumericDate . flip addUTCTime iat .
    fromInteger . (+ 86400) $ n)

genNumDate ::
  ( MonadGen n
  )
  => n NumericDate
genNumDate = NumericDate . utc <$> Gen.integral (Range.linear 1 31)
  where utc a = UTCTime (fromGregorian 2018 11 a) (secondsToDiffTime 1)

--TODO: check if multi `aud` URLs are allowed.
genAud::
  ( MonadGen n
  , MonadThrow n
  )
  => n Audience
genAud = Audience <$> Gen.list (Range.linear 10 10) genStringOrUri

genJti ::
  ( MonadGen n)
  => n JTI
genJti  = JTI <$> genText

genMeta ::
  ( MonadGen n
  , MonadThrow n
  )
  => n ClientMetaData
genMeta =
  ClientMetaData <$>
    genAlg <*>
    Gen.maybe genApplicationType <*>
    genAuthMeth <*>
    Gen.maybe genGrantTypes <*>
    Gen.maybe genScript <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genContacts <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genScriptUri <*>
    Gen.maybe genSubjectType<*>
    Gen.maybe genHttpsUrl <*>
    Gen.maybe genJwks <*>
    Gen.maybe genRequestUris <*>
    genRedirectUrls <*>
    Gen.maybe (RequestObjectEncryption <$> genAlg <*> Gen.maybe genEnc)     <*>
    Gen.maybe genAlg <*>
    Gen.maybe (IdTokenEncryption <$> genAlg <*> Gen.maybe genEnc) <*>
    Gen.maybe genResponseTypes <*>
    Gen.maybe (DefaultMaxAge <$> Gen.int (Range.linear 1 10000)) <*>
    Gen.maybe Gen.bool <*>
    Gen.maybe genAcr <*>
    Gen.maybe genHttpsUrl <*>
    Gen.maybe (UserInfoEncryption <$> genAlg <*> Gen.maybe genEnc)     <*>
    genAlg <*>
    Gen.maybe genScopes <*>
    Gen.maybe (SoftwareId <$> genText) <*>
    Gen.maybe (SoftwareVersion <$> genText) <*>
    Gen.maybe genNoteEndpoint

genSs::
  ( MonadGen n
  , MonadThrow n
  , MonadIO n
  )
  => n SoftwareStatement
genSs = SoftwareStatement <$> genRegClaims <*> genMeta

genAlg :: ( MonadGen n, MonadThrow n ) => n FapiPermittedAlg
genAlg = (?? BadAlgType) =<< ((^? _FapiPermittedAlg) <$> Gen.element [PS256,ES256])

genKid :: ( MonadGen n ) => n FapiKid
genKid  = FapiKid <$> genText

genGrantTypes :: ( MonadGen n , MonadThrow n ) => n FapiGrantTypes
genGrantTypes = GrantTypes (Set.fromList
  [Implicit,AuthorizationCode,RefreshToken])
    ^? _FapiGrantTypes ?? BadGrantType

genApplicationType :: ( MonadGen n , MonadThrow n ) => n FapiApplicationType
genApplicationType = (?? BadApplicationType) =<< (^? _FapiApplicationType) <$> Gen.element [Web]

genAuthMeth :: ( MonadGen n , MonadThrow n) => n FapiTokenEndpointAuthMethod
genAuthMeth = (?? BadAuthMeth) =<< (^? _FapiTokenEndpointAuthMethod ) <$> (Gen.element [PrivateKeyJwt] <*> genAlg)

genScript :: ( MonadGen n ) => n Script
genScript = Script DefaultLang <$> genText

genScriptUri :: ( MonadGen n , MonadThrow n ) => n ScriptUri
genScriptUri = ScriptUri DefaultLang <$> genUri

genContacts :: ( MonadGen n ) => n RegistrationContacts
genContacts = RegistrationContacts <$> Gen.set (Range.linear 10 10) (EmailAddress <$> genText)

genSubjectType :: ( MonadGen n ) => n SubjectType
genSubjectType = Gen.element [Pairwise]

genJwks :: ( MonadGen n , MonadThrow n ) => n JwkSet
genJwks = Gen.choice [JwksRef . JwksUri <$> genUri , JwksVal <$> genText]

genRequestUris :: ( MonadGen n , MonadThrow n ) => n RequestUris
genRequestUris = RequestUris <$> (Set.fromList . map RequestUri <$> Gen.list (Range.linear 10 10) genUri)

genEnc :: ( MonadGen n, MonadThrow n) => n FapiEnc
genEnc = (^? _FapiEnc)<$>Gen.element [JWE.A128CBC_HS256 , JWE.A192CBC_HS384 , JWE.A256CBC_HS512 , JWE.A128GCM , JWE.A192GCM , JWE.A256GCM]
  >>= (?? BadEncType)

genResponseTypes :: ( MonadGen n ) => n FapiResponseTypes
genResponseTypes = pure $ FapiResponseTypes CodeIdToken

genSubs  :: ( MonadGen n) => [a] -> n [a]
genSubs as = Gen.shuffle as >>= Gen.subsequence

genAcr :: ( MonadGen n) => n FapiAcrValues
genAcr = FapiAcrValues <$> genText

genScopes :: ( MonadGen n ) => n FapiScopes
genScopes = FapiScopes . mkScopes . Set.fromList <$> genSubs  [ ProfileScope ]

genText :: ( MonadGen n ) => n Text
genText = Gen.text (Range.linear 10 10) Gen.unicode

genBytes :: ( MonadGen n ) => n ByteString
genBytes = Gen.bytes (Range.linear 10 10)

genRedirectUrls::
  ( MonadGen n
  , MonadThrow n
  )
  => n RedirectUrls
genRedirectUrls = do
  https <- mkScheme "https"
  lhAutho <- (\l -> Authority Nothing l Nothing) <$> mkHost "localhost"
  autho <-Gen.filter (/=lhAutho) genAuthority
  mRedirectUris <- (^? _RedirectUrls) . Set.fromList . map RedirectUri <$> genUrls https autho
  maybe (throwM BadRedirectUri) pure mRedirectUris

genInvalidRedirectUris::
  ( MonadGen n
  , MonadThrow n
  )
  => n (Set RedirectUri)
genInvalidRedirectUris = do
  let
    goodScheme = mkScheme "https"
    badAutho = (\l -> Authority Nothing l Nothing) <$> mkHost "localhost"
    sch = Gen.choice [goodScheme,genScheme]
    autho = Gen.choice [genAuthority,badAutho]
    --filter for those that are https and !localhost (i.e both valid)
    bothvalidf gs ba = Gen.filter (\(s,a) ->not (s == gs && a /= ba))
  gs <- goodScheme
  ba <- badAutho
  (bscheme,bautho) <- bothvalidf gs ba $ (,) <$> sch <*> autho
  Set.fromList . (map RedirectUri) <$> genUrls bscheme bautho

genNoteEndpoint :: ( MonadGen n, MonadThrow n) => n NotificationEndpoint
genNoteEndpoint = NotificationEndpoint <$> genHttpsUrl

(??) :: forall m e a.  (MonadThrow m, Exception e) => Maybe a -> e ->  m a
(??) m e = maybe (throwM e) pure m
infixl 7 ??

data BadRedirectUri = BadRedirectUri deriving (Show)
instance Exception BadRedirectUri

data BadUri = BadUri deriving (Show)
instance Exception BadUri

data BadGrantType = BadGrantType deriving (Show)
instance Exception BadGrantType

data BadApplicationType = BadApplicationType  deriving (Show)
instance Exception BadApplicationType

data BadAlgType = BadAlgType   deriving (Show)
instance Exception BadAlgType

data BadEncType = BadEncType deriving (Show)
instance Exception BadEncType

data BadAuthMeth = BadAuthMeth  deriving (Show)
instance Exception BadAuthMeth

data BadResponseType = BadResponseType  deriving (Show)
instance Exception BadResponseType
