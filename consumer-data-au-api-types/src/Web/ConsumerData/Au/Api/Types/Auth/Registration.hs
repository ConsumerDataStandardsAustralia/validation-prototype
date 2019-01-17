{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.ConsumerData.Au.Api.Types.Auth.Registration
  ( RegistrationRequest(..)
  , JwsRegisteredClaims(..)
  , JwsHeaders(..)
  , ClientMetaData(..)
    -- TODO: Not exporting data constructor for RegoReqSoftwareStatement --- use the smart constructor
  , RegoReqSoftwareStatement(..)
  , SoftwareStatement(..)
  , JTI(..)
  , RegReqAccessToken(..)
  , FapiTokenEndpointAuthMethod(..)
  , Script(..)
  , Language(..)
  , SubjectType(..)
  , EmailAddress(..)
  , ScriptUri(..)
  , RegistrationContacts(..)
  , JwkSet(..)
  , RequestUris(..)
  , FapiEnc
  , _FapiEnc
  , FapiResponseTypes(..)
  , FapiApplicationType(..)
  , FapiGrantTypes
  , FapiAcrValues(..)
  , FapiScopes(..)
  , FapiKid(..)
  , RedirectUrls(..)
  , RequestObjectEncryption(..)
  , IdTokenEncryption(..)
  , DefaultMaxAge(..)
  , UserInfoEncryption(..)
  , NotificationEndpoint(..)
  , SoftwareId(..)
  , SoftwareVersion(..)
  , GrantType(..)
  , _FapiApplicationType
  , ApplicationType(..)
  , TokenEndpointAuthMethod(..)
  , RequestUri(..)
  , _RedirectUrls
  , aesonClaimsToMetaData
  , metaDataToAesonClaims
  , regoReqToJwt
  , jwtToRegoReq
  , JwksUri(..)
  , _FapiTokenEndpointAuthMethod
  , GrantTypes(..)
  , _RegistrationErrorType
  , RegistrationErrorDescription(..)
  , RegistrationErrorType(..)
  , RegistrationError(..)
  , ClientIdIssuedAt(..)
  , ClientSecretExpiresAt(..)
  , ClientSecret(..)
  , RegistrationClientUri(..)
  , RegistrationAccessToken(..)
  , RegistrationResponse(..)
  , requestObjectSigningAlg
  , applicationType
  , tokenEndpointAuthMethod
  , grantTypes
  , clientName
  , contacts
  , logoUri
  , policyUri
  , tosUri
  , subjectType
  , sectorIdentifierUri
  , keySet
  , requestUris
  , redirectUris
  , requestObjectEncryption
  , userinfoSignedResponseAlg
  , idTokenEncryption
  , responseTypes
  , defaultMaxAge
  , requireAuthTime
  , defaultAcrValues
  , initiateLoginUri
  , userInfoEncryption
  , idTokenSignedResponseAlg
  , scope
  , softwareId
  , softwareVersion
  , clientNotificationEndpoint
  , _FapiGrantTypes
  , clientMetaData
  , softwareStatement
  , iss
  , aud
  , iat
  , exp
  , jti
  , _EncodedSs
  , _DecodedSs
  , ssSigningData
  , ssMetaData
  )
where

import           Aeson.Helpers
    (parseJSONWithPrism, parseSpaceSeperatedSet, parseWithPrism,
    toJsonSpaceSeperatedSet, _URI)
import           Control.Applicative                       (liftA2, (<|>))
import           Control.Lens
    (Prism', at, makeLenses, makePrisms, makeWrapped, prism, prism', to, ( # ),
    (&), (.~), (?~), (^.), (^?), _Right)
import           Control.Lens.Combinators                  (_Just)
import           Control.Lens.Wrapped                      (_Unwrapped)
import           Control.Monad                             (join)
import           Control.Monad.Error.Class
    (MonadError, throwError)
import           Control.Monad.Error.Lens                  (throwing)
import           Control.Monad.Time                        (MonadTime)
import qualified Crypto.JOSE.Error                         as JE
import           Crypto.JOSE.JWA.JWE                       (Enc)
import qualified Crypto.JOSE.JWA.JWE                       as JWE
import           Crypto.JOSE.JWK                           (JWK)
import           Crypto.JOSE.JWS
    (HeaderParam (..), JWSHeader, ProtectionIndicator, alg, header, kid,
    newJWSHeader, signatures)
import           Crypto.JWT
    (AsJWTError, Audience, ClaimsSet, NumericDate, SignedJWT, StringOrURI,
    claimAud, claimExp, claimIat, claimIss, claimJti, decodeCompact,
    defaultJWTValidationSettings, emptyClaimsSet, encodeCompact,
    issuerPredicate, param, signClaims, unregisteredClaims, verifyClaims)
import           Crypto.Random.Types                       (MonadRandom)
import           Data.Aeson
    (FromJSON (..), Result (..), ToJSON (..), Value (..), fromJSON)
import qualified Data.ByteString.Lazy                      as BSL
    (fromStrict, toStrict)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as M
import           Data.Maybe                                (isJust)
import           Data.Set                                  (Set, isSubsetOf)
import qualified Data.Set                                  as Set
import qualified Data.Text                                 as T
import           Data.Text.Encoding                        as TE
    (decodeUtf8, encodeUtf8)
import           GHC.Generics                              (Generic)
import           Prelude                                   hiding (exp)
import           Text.URI                                  (URI, render)
import qualified Text.URI                                  as URI
import           Text.URI.Lens
    (authHost, uriAuthority, uriScheme)
import           Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientId, ClientIss (..), FapiPermittedAlg, HttpsUrl, RedirectUri,
    ResponseType, Scopes, getRedirectUri, _FapiPermittedAlg)
import           Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsError, Error, _InvalidClaim, _MissingClaim, _ParseError)
-- | The client registration endpoint is an OAuth 2.0 endpoint that is designed to allow a client to be dynamically registered with the authorization server. 'RegistrationRequest' represents a client request for registration containing meta-data elements specified in <https://tools.ietf.org/html/rfc7591 §RFC7591 - OAuth 2.0 Dynamic Client Registration Protocol> and <https://openid.net/specs/openid-connect-registration-1_0.html §OIDC registration>. Each request must contain a software statement assertion (a JWT is issued and signed by the OpenBanking Directory). Metadata values may be duplicated in the registration request, but if different, those in the software statement will take precedence and override those in the request.
--
--A RP will submit the registration request to a OP in order to receive `client_id` credentials. The registration request parameters are sent in a JWT signed by the RP [UK OB mandates this] and include the signed software statement assertion as a JWT claim.
--
-- Full details of CDR dynamic client registration can be found in the <https://consumerdatastandardsaustralia.github.io/infosec/#discovery-and-registration §CDR infosec standards>.

-- | The client registration endpoint is an OAuth 2.0 endpoint that is designed to
-- allow a client to be dynamically registered with the authorization server.
-- 'RegistrationRequest' represents a client request for registration containing
-- meta-data elements specified in
-- <https://tools.ietf.org/html/rfc7591 §RFC7591 - OAuth 2.0 Dynamic Client Registration Protocol>
-- and <https://openid.net/specs/openid-connect-registration-1_0.html §OIDC registration>.
-- Each request must contain a software statement assertion (a
-- JWT is issued and signed by the OpenBanking Directory). Metadata values may
-- be duplicated in the registration request, but if different, those in the
-- software statement will take precedence and override those in the request. A
-- RP will submit the registration request to a OP in order to receive
-- `client_id` credentials. The registration request parameters are sent in a
-- JWT signed by the RP [UK OB mandates this] and include the signed software
-- statement assertion as a JWT claim. Full details of CDR dynamic client
-- registration can be found in the
-- <https://consumerdatastandardsaustralia.github.io/infosec/#discovery-and-registration §CDR infosec standards>.

-- | Determines the set of credentials that will be used by a client when
-- accessing /token endpoint. OIDC requires a restricted subset of the allowed
-- OAuth values (<https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>).
-- Also, because OZ OB only supports @code id_token@
-- (i.e. the `hybrid` flow), this means @grant_types@ must contain at least
-- @authorization_code@ and @implicit@, as per the OIDC Registration spec
-- (<https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>).
-- NB: For UK OB, @client_credentials@ is required as a grant type,
-- as is it needed for the client to submit a JWT to the /token endpoint to
-- obtain a consent ID / payment ID before sending the request to /payments.
data GrantType = Implicit | AuthorizationCode | RefreshToken -- ClientCredentials
  deriving (Generic, Show, Eq, Ord)

_GrantType :: Prism' T.Text GrantType
_GrantType = prism
  (\case
    Implicit          -> "implicit"
    AuthorizationCode -> "authorization_code"
    RefreshToken      -> "refresh_token"
  )
  (\case
    "implicit"           -> Right Implicit
    "authorization_code" -> Right AuthorizationCode
    "refresh_token"      -> Right RefreshToken
    t                    -> Left t
  )

instance ToJSON GrantType where
  toJSON = toJSON . (_GrantType #)

instance FromJSON GrantType where
  parseJSON = parseJSONWithPrism _GrantType "GrantType"

newtype GrantTypes = GrantTypes (Set GrantType)
  deriving (Generic, Show, Eq)

instance ToJSON GrantTypes where
  toJSON (GrantTypes s) = toJsonSpaceSeperatedSet (_GrantType #) s

instance FromJSON GrantTypes where
  parseJSON = fmap GrantTypes . parseSpaceSeperatedSet _GrantType "GrantType"

newtype FapiGrantTypes = FapiGrantTypes GrantTypes
  deriving (Generic, Show, Eq)

instance ToJSON FapiGrantTypes where
  toJSON = toJSON . (_FapiGrantTypes #)

instance FromJSON FapiGrantTypes where
  parseJSON = parseJSONWithPrism _FapiGrantTypes  "FapiGrantTypes"

_FapiGrantTypes :: Prism' GrantTypes FapiGrantTypes
_FapiGrantTypes = prism'
  (\(FapiGrantTypes f) -> f)
  (\(GrantTypes grantTypes) ->
    if grantTypes
         `isSubsetOf` permittedGrantTypes
         &&           requiredGrantTypes
         `isSubsetOf` grantTypes
      then Just . FapiGrantTypes . GrantTypes $ grantTypes
      else Nothing
  )
 where
  permittedGrantTypes =
    Set.fromList [Implicit, AuthorizationCode, RefreshToken]
  requiredGrantTypes = Set.fromList [Implicit, AuthorizationCode]

data ApplicationType = Web | Native
  deriving (Generic, Show, Eq)

_ApplicationType :: Prism' T.Text ApplicationType
_ApplicationType = prism
  (\case
    Web    -> "web"
    Native -> "native"
  )
  (\case
    "web"    -> Right Web
    "native" -> Right Native
    t        -> Left t
  )

instance ToJSON ApplicationType where
  toJSON = toJSON . (_ApplicationType #)

instance FromJSON ApplicationType where
  parseJSON = parseJSONWithPrism _ApplicationType "ApplicationType"

newtype FapiApplicationType = FapiApplicationType ApplicationType
  deriving (Generic, ToJSON, Show, Eq)

instance FromJSON FapiApplicationType where
  parseJSON = parseJSONWithPrism _FapiApplicationType  "FapiApplicationType"

-- | Smart constructor for producing FAPI permitted @application_type@s. In the
-- current version of CDR, only @web@ is allowed.
_FapiApplicationType :: Prism' ApplicationType FapiApplicationType
_FapiApplicationType = prism'
  (\(FapiApplicationType a) -> a)
  (\case
    Web -> Just . FapiApplicationType $ Web
    _   -> Nothing
  )

newtype EmailAddress = EmailAddress {
    fromEmailAddress :: T.Text
  } deriving (Ord, Show, Eq)

newtype RegistrationContacts = RegistrationContacts (Set EmailAddress)
  deriving (Generic, Show, Eq)

instance ToJSON RegistrationContacts where
  toJSON (RegistrationContacts s) = toJsonSpaceSeperatedSet fromEmailAddress s

instance FromJSON RegistrationContacts where
  parseJSON = fmap RegistrationContacts . parseSpaceSeperatedSet _Unwrapped "EmailAddress"

-- | Text with support for BCP47 [RFC5646] language tags in keys. e.g.
-- client_name#ja-Jpan-JP :: クライアント名", as required by
-- https://openid.net/specs/openid-connect-registration-1_0.html#LanguagesAndScripts.
data Script = Script Language T.Text
  deriving (Generic, Show, Eq)

instance ToJSON Script where
  toJSON (Script _ t) = toJSON t

instance FromJSON Script where
  parseJSON v = Script DefaultLang <$> parseJSON v

data Language = DefaultLang
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data ScriptUri = ScriptUri Language URI
  deriving (Generic, Show, Eq)

instance ToJSON ScriptUri where
  toJSON (ScriptUri _ uri) = toJSON uri

instance FromJSON ScriptUri where
  parseJSON = fmap (ScriptUri DefaultLang) . parseJSON

-- | @subject_type@ requested for responses to the client; only @pairwise@ is
-- supported in <https://consumerdatastandardsaustralia.github.io/infosec/#data-holder-metadata §CDR>
data SubjectType = Pairwise -- `Public` type not supported
  deriving (Generic, Show, Eq)

_SubjectType :: Prism' T.Text SubjectType
_SubjectType = prism
  (\case
    Pairwise -> "pairwise"
  )
  (\case
    "pairwise" -> Right Pairwise
    t          -> Left t
  )

instance ToJSON SubjectType where
  toJSON = toJSON . (_SubjectType #)

instance FromJSON SubjectType where
  parseJSON = parseJSONWithPrism _SubjectType "SubjectType"

newtype JwksUri = JwksUri URI
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

--TODO use keyset instead of Text for JwksVal
-- | Client's JSON Web Key Set
data JwkSet =
  -- | @jwks_uri@ parameter that specifies a URL for the client's JSON Web Key
  -- Set for pass-by-reference

  JwksRef JwksUri
  -- | @jwks@ parameter that specifies client's JSON Web Key Set document
  -- (passed by value). It is recommended [OIDC-R] that keys are passed by
  -- reference if possible.
  | JwksVal T.Text
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

jwkSetClaims :: Maybe JwkSet -> AesonClaims
jwkSetClaims = \case
  Just (JwksVal v) -> M.empty & at "jwks" ?~ toJSON v
  Just (JwksRef u) -> M.empty & at "jwks_uri" ?~ toJSON u
  Nothing          -> M.empty

-- | JWE @alg@ (and optional @enc@) algorithms for encrypting the ID token
-- issued to the client (server must comply). @none@ is not permitted, unless
-- the client only uses response types that return no ID token from the
-- authorization endpoint (such as when only using the authorization code flow).
-- This is not currently an option in OB.
--
-- Default @alg@ is @PS256@. If only the @id_token_encrypted_response_alg@ is
-- specified, the default for @enc@ is @A128CBC-HS256@.
data IdTokenEncryption = IdTokenEncryption
  {
    idTokenAlg :: FapiPermittedAlg
  , idTokenEnc :: Maybe FapiEnc
  }
  deriving (Generic, Show, Eq)

-- | JWE @alg@ (and optional @enc@) algorithms for encrypting the UserInfo
-- response sent to the client (server must comply). @none@ is not permitted,
-- unless the client only uses response types that return no ID token from the
-- authorization endpoint (such as when only using the authorization code flow).
-- This is not currently an option in OB.
--
-- Default @alg@ is @PS256@. If only the @id_token_encrypted_response_alg@ is
-- specified, the default for @enc@ is @A128CBC-HS256@.
data UserInfoEncryption = UserInfoEncryption
  {
    userInfoAlg :: FapiPermittedAlg
  , userInfoEnc :: Maybe FapiEnc
  }
  deriving (Generic, Show, Eq)

data RequestObjectEncryption = RequestObjectEncryption
  {
    reqObjAlg :: FapiPermittedAlg
  , reqObjEnc :: Maybe FapiEnc
  }
  deriving (Generic, Show, Eq)

data TokenEndpointAuthMethod = ClientSecretPost
                             | ClientSecretBasic
                             | ClientSecretJwt FapiPermittedAlg
                             | PrivateKeyJwt FapiPermittedAlg
                             | TlsClientAuth TlsClientAuthSubjectDn
                             | None
  deriving (Generic, Show, Eq)

_TokenEndpointAuthMethod :: Prism' AesonClaims TokenEndpointAuthMethod
_TokenEndpointAuthMethod = prism
  tokEndPtMethMap
  (\m -> either (const $ Left m)
                Right
                (getTokEndPtMeth m :: Either Error TokenEndpointAuthMethod)
  )

tokEndPtMethMap :: TokenEndpointAuthMethod -> AesonClaims
tokEndPtMethMap = \case
  ClientSecretPost  -> setApm' "client_secret_post"
  ClientSecretBasic -> setApm' "client_secret_basic"
  ClientSecretJwt j ->
    setApm ("token_endpoint_auth_method", "client_secret_jwt" :: T.Text)
      <> setApm ("token_endpoint_auth_signing_alg", j)
  PrivateKeyJwt j ->
    setApm ("token_endpoint_auth_method", "private_key_jwt" :: T.Text)
      <> setApm ("token_endpoint_auth_signing_alg", j)
  TlsClientAuth t ->
    setApm ("token_endpoint_auth_method", "tls_client_auth" :: T.Text)
      <> setApm ("tls_client_auth_subject_dn", t)
  None -> setApm' "none"
 where
  setApm :: ToJSON a => (T.Text, a) -> AesonClaims
  setApm (k, a) = M.empty & at k ?~ toJSON a
  setApm' a = M.empty & at "token_endpoint_auth_method" ?~ toJSON (a :: T.Text)

getTokEndPtMeth
  :: forall e m
   . (AsError e, MonadError e m)
  => AesonClaims
  -> m TokenEndpointAuthMethod
getTokEndPtMeth m = do
  meth <- getClaim m "token_endpoint_auth_method"
  case meth :: T.Text of
    "client_secret_post"  -> pure ClientSecretPost
    "client_secret_basic" -> pure ClientSecretBasic
    "client_secret_jwt" ->
      ClientSecretJwt <$> getClaim m "token_endpoint_auth_signing_alg"
    "private_key_jwt" ->
      PrivateKeyJwt <$> getClaim m "token_endpoint_auth_signing_alg"
    "tls_client_auth" ->
      TlsClientAuth <$> getClaim m "tls_client_auth_subject_dn"
    "none" -> pure None
    _      -> throwing _ParseError "Invalid token_endpoint_auth_method"

instance ToJSON TokenEndpointAuthMethod where
  toJSON = toJSON . (_TokenEndpointAuthMethod #)

instance FromJSON TokenEndpointAuthMethod where
  parseJSON = parseJSONWithPrism _TokenEndpointAuthMethod "TokenEndpointAuthMethod"

-- | A string representation of the expected subject distinguished name of the
-- certificate the OAuth client will use in mutual TLS authentication.
newtype TlsClientAuthSubjectDn = TlsClientAuthSubjectDn T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Only PrivateKeyJwt and TlsClientAuth are supported by CDR.
-- @token_endpoint_auth_signing_alg@ is required if using @PrivateKeyJwt@
-- @token_endpoint_auth_method@, and @tls_client_auth_subject_dn@ must be
-- supplied if using @tls_client_auth@ (as per
-- <https://consumerdatastandardsaustralia.github.io/infosec/#recipient-client-registration §CDR Registration>).
-- All token requests will be rejected by the server if they are not signed by
-- the algorithm specified in @alg@, or if they are signed with @none@, or if
-- the subject distinguished name of the certificate does not match that of the
-- MTLS certificate.
newtype FapiTokenEndpointAuthMethod = FapiTokenEndpointAuthMethod TokenEndpointAuthMethod
  deriving (Generic, Show, Eq)

_FapiTokenEndpointAuthMethod
  :: Prism' TokenEndpointAuthMethod FapiTokenEndpointAuthMethod
_FapiTokenEndpointAuthMethod = prism
  (\case
    FapiTokenEndpointAuthMethod f -> f
  )
  (\case
    -- ClientSecretJwt f -> Right . FapiTokenEndpointAuthMethod . ClientSecretJwt $ f -- Not supported in CDR.
    PrivateKeyJwt f -> Right . FapiTokenEndpointAuthMethod . PrivateKeyJwt $ f
    TlsClientAuth f -> Right . FapiTokenEndpointAuthMethod . TlsClientAuth $ f
    e               -> Left e
  )

-- | FAPI accepted algorithms for content encryption, based on
-- <https://tools.ietf.org/html/rfc7518 §RFC 7518 5. Cryptographic Algorithms for Content Encryption>.
data FapiEnc =
      A128CBC_HS256
    | A192CBC_HS384
    | A256CBC_HS512
    | A128GCM
    | A192GCM
    | A256GCM
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: make iso if all enc's supported
_FapiEnc :: Prism' Enc FapiEnc
_FapiEnc = prism'
  (\case
    A128CBC_HS256 -> JWE.A128CBC_HS256 -- Unsure if the CBC ciphers are acceptable
    A192CBC_HS384 -> JWE.A192CBC_HS384
    A256CBC_HS512 -> JWE.A256CBC_HS512
    A128GCM       -> JWE.A128GCM
    A192GCM       -> JWE.A192GCM
    A256GCM       -> JWE.A256GCM
  )
  (\case
    JWE.A128CBC_HS256 -> Just A128CBC_HS256
    JWE.A192CBC_HS384 -> Just A192CBC_HS384
    JWE.A256CBC_HS512 -> Just A256CBC_HS512
    JWE.A128GCM       -> Just A128GCM
    JWE.A192GCM       -> Just A192GCM
    JWE.A256GCM       -> Just A256GCM
            -- e -> Left e
  )

-- | @default_max_age@, in seconds.
newtype DefaultMaxAge  = DefaultMaxAge Int
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | @jti@ JWT param
newtype JTI = JTI {
  getJTI::T.Text
                  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype RequestUris = RequestUris {
  getRequestUris  :: Set RequestUri}
  deriving (Generic, Show, Eq)

instance ToJSON RequestUris where
  toJSON (RequestUris set) =
    toJsonSpaceSeperatedSet (render . getRequestUri) set

instance FromJSON RequestUris where
  parseJSON =
    fmap RequestUris . parseSpaceSeperatedSet (_URI . _Unwrapped) "RequestUris"

newtype RequestUri =
  RequestUri {getRequestUri :: URI}
  deriving (Generic, Show, Eq, ToJSON, FromJSON, Ord)

-- | 'RedirectUrls' is a non-empty array of redirection URI values used by the
-- client to match against supplied @redirect_uri@ request parameter. If using
-- @web@ scheme (as per CDR) these must all be HTTPS, and must not use
-- 'localhost' as hostname.
newtype RedirectUrls = RedirectUrls {
  getRedirectUrls  :: Set RedirectUri}
  deriving (Generic, Show, Eq)

instance ToJSON RedirectUrls where
  toJSON (RedirectUrls set) =
    toJsonSpaceSeperatedSet (render . getRedirectUri) set

instance FromJSON RedirectUrls where
  parseJSON v = parseWithPrism _RedirectUrls "RedirectUrls"
    =<< parseSpaceSeperatedSet (_URI . _Unwrapped) "RedirectUri" v

-- | Constructor for @redirect_url@ array; all URLs must be HTTPS, none may be
-- localhost, as mandated by CDR.
_RedirectUrls :: Prism' (Set RedirectUri) RedirectUrls
_RedirectUrls = prism'
  (\(RedirectUrls r) -> r)
  (\uris -> if not (null uris) && allValid uris
    then Just . RedirectUrls $ uris
    else Nothing
  )
 where
  isValidHost uri = and $ liftA2 (/=)
                                 (URI.mkHost "localhost")
                                 (uri ^? uriAuthority . _Right . authHost)
  isHttps uri = and $ liftA2 (==) (URI.mkScheme "https") (uri ^. uriScheme)
  allValid = all (liftA2 (&&) isValidHost isHttps . getRedirectUri)

-- TODO: prism to enforce only 'code id_token' for future proofing.
-- | The only CDR acceptable value is @code id_token@
newtype FapiResponseTypes = FapiResponseTypes ResponseType
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype FapiScopes = FapiScopes Scopes
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

--TODO: use Auth.Common's values here: must contain @urn:cds.au:cdr:3@ at a minimum.
--TODO: should be building acr's from JSON with a prism
newtype FapiAcrValues = FapiAcrValues T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype NotificationEndpoint = NotificationEndpoint HttpsUrl
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data RegistrationRequest = RegistrationRequest {
   _clientMetaData     :: ClientMetaData
  -- | A signed JWT containing metadata about the client software. RFC7591
  -- mandates that this is a JWS.
  , _softwareStatement :: RegoReqSoftwareStatement
} deriving (Generic, Show, Eq)

--TODO better type only allowing {YYYY-MM-DD | YYYY-MM-DD.<V>} format.
-- | This key ID must be the value of the date on which the key was published in
-- the format YYYY-MM-DD and must be unique within the set. If more than one key
-- is published on the same date, the kid must consist of the following format
-- YYYY-MM-DD.<V> where <V> represents an increasing version number and positive
-- integer. See the
-- <https://consumerdatastandardsaustralia.github.io/infosec/#json-web-key-sets §infosec spec>
-- for more info.
newtype FapiKid = FapiKid {
  getFapiKid :: T.Text }
  deriving (Generic, Show, Eq)

-- | These claims are JWS registered claims required for the registration
-- requests.
data JwsRegisteredClaims = JwsRegisteredClaims
  {
  -- | The name of the RP. RFC7591 2.3 mandates issuer claim is present if
  -- supplied in a software statement.
    _iss :: Maybe ClientIss
  -- | The OP.
  , _aud :: Maybe Audience
  -- | Issued at.
  , _iat :: Maybe NumericDate
  -- | Expiration time.
  , _exp :: Maybe NumericDate
  -- | JWT ID.
  , _jti :: Maybe JTI
} deriving (Generic, Show, Eq)

data JwsHeaders = JwsHeaders
  {
  -- | The JWK @alg@ to sign the request with.
    _alg :: FapiPermittedAlg

  -- | @kid@ is a required header for
  -- <https://consumerdatastandardsaustralia.github.io/infosec/#jose-jwt-header §CDR>.
  , _kid :: FapiKid
} deriving (Generic, Show, Eq)

-- | The following claims are specified in
-- <https://tools.ietf.org/html/rfc7591 §RFC7591: 3.1 Client Registration Request>
-- to be used in both the registration request object and the software
-- statement, all of them as optional. However, some fields are non-optional
-- under FAPI, and even fewer are optional under
-- <https://consumerdatastandardsaustralia.github.io/infosec/#recipient-client-registration §CDR>
data ClientMetaData = ClientMetaData {
  -- | The `alg` algorithm that must be used for signing request objects sent to
  -- the OP.
    _requestObjectSigningAlg    :: FapiPermittedAlg

  -- | Kind of the application. CDR mandates this to be just @web@ (i.e no
  -- @native@ apps are allowed).
  , _applicationType            :: Maybe FapiApplicationType

  -- | Specifies which token endpoint authentication method the client will use,
  -- and also the algorithm (@token_endpoint_auth_signing_alg@) that must be
  -- used for signing if a JWT auth method is used.
  , _tokenEndpointAuthMethod    :: FapiTokenEndpointAuthMethod

  -- | The set of grant types that a client will restrict itself to using (see
  -- <https://openid.net/specs/openid-connect-registration-1_0.html OIDC-R 2. Client Metadata>
  -- and <https://tools.ietf.org/html/rfc7591 §RFC7591 2. -- Client Metadata>).
  -- If omitted, the default is that the client will use only the
  -- `authorization_code` grant type.
  , _grantTypes                 :: Maybe FapiGrantTypes

  -- | Human-readable name of the client to be presented to the end user.
  -- Mandatory field according to CDR.
  , _clientName                 :: Maybe Script

  -- | URL of the home page of the client.
  , _clientUri                  :: Maybe ScriptUri

  -- | Array of e-mail addresses of people responsible for the client.
  , _contacts                   :: Maybe RegistrationContacts

  -- | URL that references a logo for the client application.
  , _logoUri                    :: Maybe ScriptUri

  -- | URL that the client provides to the end user in order to read about the how
  -- the user data will be used.
  , _policyUri                  :: Maybe ScriptUri

  -- | URL that the client provides to the end user to read about the client's
  -- terms of service.
  , _tosUri                     :: Maybe ScriptUri

  -- | Requested for responses to this client. Mandatory field according to CDR.
  , _subjectType                :: Maybe SubjectType

  -- | References an HTTPS URL of a remote file containing a single JSON array of
  -- @redirect_uri@ values.
  , _sectorIdentifierUri        :: Maybe HttpsUrl

  -- | Either the @jwks_uri@ or @jwks@ parameter specifying client's JWKS.
  , _keySet                     :: Maybe JwkSet

  -- | Array of @request_uri@ values that are pre-registered by the RP for use at
  -- the OP, which may cache their contents and not retrieve them at the time
  -- they are used in a request.
  , _requestUris                :: Maybe RequestUris

  -- | Non-empty array of redirection URI values used by the client to match
  -- against supplied @redirect_uri@ request parameter. If using @web@ scheme
  -- these must all be HTTPS, and must not use 'localhost' as hostname.
  -- Mandatory field, according to OIDC-R. FAPI restricts these to all be HTTPS.
  , _redirectUris               :: RedirectUrls

  -- | The @request_object_encryption_alg@ and @request_object_encryption_enc@
  -- parameters for specifying the JWS `alg` and `enc` algorithms (that might
  -- be) used when encrypting request objects. If both signed and encrypted,
  -- signing will occur first, and then encryption, with the result being a
  -- nested JWT. Warning: a RP can supply unencrypted requests, even if this is
  -- present, as this is only a declaration that the RP 'might' encrypt request
  -- objects. See 'RequestObjectEncryption' for more info.
  , _requestObjectEncryption    :: Maybe RequestObjectEncryption

  -- | The @userinfo_signed_response_alg@ parameter for specifying which JWS `alg`
  -- algorithm should be used for signing UserInfo responses.
  , _userinfoSignedResponseAlg  :: Maybe FapiPermittedAlg

  -- | The @id_token_encrypted_response_alg@ and @id_token_encrypted_response_enc@
  -- values for specifying how the ID token should be encrypted; see
  -- 'IdTokenEncryption' for more information. Mandatory field according to CDR.
  , _idTokenEncryption          :: Maybe IdTokenEncryption

  -- | JSON array containing a list of OAuth 2.0 @response_type@ values that the
  -- client will restrict itself to using; defaults to @code id_token@ if
  -- omitted. FAPI restricts these to be either `code id_token` or `code
  -- id_token token`.
  , _responseTypes              :: Maybe FapiResponseTypes

  -- | Specifies that the end user must be actively authenticated if the end user
  -- was authenticated longer than the specified number of seconds ago; the
  -- @max_age@ request object parameter overides this.
  , _defaultMaxAge              :: Maybe DefaultMaxAge

  -- | Whether the @auth_time@ claim in the ID token is required, defaults to
  -- False.
  , _requireAuthTime            :: Maybe Bool

  -- | Default 'Authentication Context Class Reference' (ACR) values for requests,
  -- values ordered by preference; defaults overridden by supplied values.
  , _defaultAcrValues           :: Maybe FapiAcrValues

  -- | HTTS URL which can be used by a third-party to initiate a login by the RP.
  , _initiateLoginUri           :: Maybe HttpsUrl

  -- | The @userinfo_encrypted_response_alg@ and @userinfo_encrypted_response_enc@
  -- values for specifying how UserInfo response should be encrypted; see
  -- 'UserInfoEncryption' for more information.
  , _userInfoEncryption         :: Maybe UserInfoEncryption

  -- | The `alg` for signing the ID token issued to this client. NB: OIDC-R allows
  -- this to be optional (see
  -- <https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>),
  -- but by using FAPI the implication is that it is mandatory
  -- (<https://openid.net/specs/openid-financial-api-part-2.html#public-client §FAPI RW - Section 5.2.3 >).
  -- Mandatory field according to CDR.
  , _idTokenSignedResponseAlg   :: FapiPermittedAlg

  -- | A set of scopes the client will restrict itself to, containing at least @openid@.
  , _scope                      :: Maybe FapiScopes

  -- | Unique identifier string for the client, which should remain the same
  -- across all instances of the client software, and all versions of the client
  -- software. This must match the software ID in the SSA if supplied.
  , _softwareId                 :: Maybe SoftwareId

  -- | The version number of the software should a TPP choose to register and / or
  -- maintain it.
  , _softwareVersion            :: Maybe SoftwareVersion

  -- | @client_notification_endpoint@ - CDR required URI for CIBA callback.
  , _clientNotificationEndpoint :: Maybe NotificationEndpoint
} deriving (Generic, Show, Eq)

-- TODO: Add smart constructor to only allow supply of encoded SS, depending on the CDR spec
data RegoReqSoftwareStatement = EncodedSs T.Text | DecodedSs SoftwareStatement
  deriving (Show, Eq)

data SoftwareStatement = SoftwareStatement {
    _ssSigningData :: JwsRegisteredClaims
  , _ssMetaData    :: ClientMetaData -- ^ All the claims to include in the SSA; RFC7591 allows any/all that are included in the request object, however SSA claims take precedence over request object claims (3.1.1).
}
  deriving (Generic, Show, Eq)

newtype SoftwareId = SoftwareId T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
newtype SoftwareVersion = SoftwareVersion T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Response type for a dynamic registration request. NB: A successful response
-- will return a HTTP 201
-- (<https://openid.net/specs/openid-connect-registration-1_0.html §3.2. Client Registration Response>)
data RegistrationResponse = RegistrationResponse
  {
  -- | OAuth 2.0 unique client identifier string. RFC7591 mandated.
    _clientId                 :: ClientId
  -- | OAuth 2.0 unique client secret string, used by confidential clients to
  -- authenticate to token end-point. NB: This will only ever be used by the
  -- `client_secret_jwt` client auth method.
  , _clientSecret             :: Maybe ClientSecret
  -- | Time at which the client identifier was issued.
  , _clientIdIssuedAt         :: Maybe ClientIdIssuedAt
  -- | URI and token to perform further operations if necessary.
  , _regReqAccessToken        :: Maybe RegReqAccessToken
  -- | The rego server must return all registered metadata about the client
  -- (OIDC-R 3.2. Client Registration Response).
  , _regRespClientMetaData    :: ClientMetaData
  -- | RFC7591 3.2.1 mandates software statement supplied in a request must be
  -- returned unmodified
  , _regRespSoftwareStatement :: SoftwareStatement
}

-- | Registration access token and location of URI that can be used by the client
-- to perform further operations upon the client registration.
data RegReqAccessToken =
  RegReqAccessToken RegistrationAccessToken RegistrationClientUri
newtype RegistrationAccessToken = RegistrationAccessToken T.Text
newtype RegistrationClientUri = RegistrationClientUri URI

data ClientSecret = ClientSecret T.Text ClientSecretExpiresAt
-- | Time at which the client secret will expire or 0 if it will not expire.
newtype ClientSecretExpiresAt = ClientSecretExpiresAt NumericDate

-- | Time at which the Client Identifier was issued.
newtype ClientIdIssuedAt = ClientIdIssuedAt NumericDate

data RegistrationError = RegistrationError {
   _regError            :: RegistrationErrorType
 , _regErrorDescription :: Maybe RegistrationErrorDescription
                                           }

data RegistrationErrorType =
  -- |  The value of one or more redirection URIs is invalid.
  INVALID_REDIRECT_URI
  -- | The value of one of the client metadata fields is invalid and the server
  -- has rejected this request.
  | INVALID_CLIENT_METADATA
  -- | The software statement presented is invalid.
  | INVALID_SOFTWARE_STATEMENT
  -- | The software statement presented is not approved for use by this
  -- authorization server.
  | UNAPPROVED_SOFTWARE_STATEMENT

newtype RegistrationErrorDescription = RegistrationErrorDescription T.Text

_RegistrationErrorType :: Prism' T.Text RegistrationErrorType
_RegistrationErrorType = prism
  (\case
    INVALID_REDIRECT_URI          -> "invalid_redirect_uri"
    INVALID_CLIENT_METADATA       -> "invalid_client_metadata"
    INVALID_SOFTWARE_STATEMENT    -> "invalid_software_statement"
    UNAPPROVED_SOFTWARE_STATEMENT -> "unapproved_software_statement"
  )
  (\case
    "invalid_redirect_uri"          -> Right INVALID_REDIRECT_URI
    "invalid_client_metadata"       -> Right INVALID_CLIENT_METADATA
    "invalid_software_statement"    -> Right INVALID_SOFTWARE_STATEMENT
    "unapproved_software_statement" -> Right UNAPPROVED_SOFTWARE_STATEMENT
    e                               -> Left e
  )

type AesonClaims = HashMap T.Text Value

-- Jose needs JWT claims to be supplied as Aeson Values
metaDataToAesonClaims :: ClientMetaData -> AesonClaims
metaDataToAesonClaims ClientMetaData {..} =
  M.empty
    &  at "client_name"
    .~ (toJSON <$> _clientName)
    &  at "client_uri"
    .~ (toJSON <$> _clientUri)
    &  at "contacts"
    .~ (toJSON <$> _contacts)
    &  at "logo_uri"
    .~ (toJSON <$> _logoUri)
    &  at "policy_uri"
    .~ (toJSON <$> _policyUri)
    &  at "tos_uri"
    .~ (toJSON <$> _tosUri)
    &  at "subject_type"
    .~ (toJSON <$> _subjectType)
    &  at "sector_identifier_uri"
    .~ (toJSON <$> _sectorIdentifierUri)
    &  (<> jwkSetClaims _keySet)
    &  at "request_uris"
    .~ (toJSON <$> _requestUris)
    &  at "redirect_uris"
    ?~ toJSON _redirectUris
    &  at "request_object_encryption_alg"
    .~ (toJSON . reqObjAlg <$> _requestObjectEncryption)
    &  at "request_object_encryption_enc"
    .~ (toJSON <$> (reqObjEnc =<< _requestObjectEncryption))
    &  at "userinfo_signed_response_alg"
    .~ (toJSON <$> _userinfoSignedResponseAlg)
    &  at "id_token_encrypted_response_alg"
    .~ (toJSON . idTokenAlg <$> _idTokenEncryption)
    &  at "id_token_encrypted_response_enc"
    .~ (toJSON <$> (idTokenEnc <$> _idTokenEncryption))
    &  at "response_types"
    .~ (toJSON <$> _responseTypes)
    &  at "default_max_age"
    .~ (toJSON <$> _defaultMaxAge)
    &  at "require_auth_time"
    .~ (toJSON <$> _requireAuthTime)
    &  at "default_acr_values"
    .~ (toJSON <$> _defaultAcrValues)
    &  at "initiate_login_uri"
    .~ (toJSON <$> _initiateLoginUri)
    &  at "user_info_encrypted_response_alg"
    .~ (toJSON . userInfoAlg <$> _userInfoEncryption)
    &  at "user_info_encrypted_response_enc"
    .~ (toJSON <$> (userInfoEnc =<< _userInfoEncryption))
    &  at "id_token_signed_response_alg"
    ?~ toJSON _idTokenSignedResponseAlg
    &  at "request_object_signing_alg"
    ?~ toJSON _requestObjectSigningAlg
    &  at "grant_types"
    .~ (toJSON <$> _grantTypes)
    &  at "application_type"
    .~ (toJSON <$> _applicationType)
    &  (<> ( _TokenEndpointAuthMethod
           . _FapiTokenEndpointAuthMethod
           # _tokenEndpointAuthMethod
           )
       )
    &  at "scope"
    .~ (toJSON <$> _scope)
    &  at "software_id"
    .~ (toJSON <$> _softwareId)
    &  at "software_version"
    .~ (toJSON <$> _softwareVersion)
    &  at "client_notification_endpoint"
    .~ (toJSON <$> _clientNotificationEndpoint)

-- | Currently all meta-data is included in the software statement.
ssToAesonClaims :: SoftwareStatement -> AesonClaims
ssToAesonClaims = metaDataToAesonClaims . _ssMetaData

-- | Sign a registration request for sending to OP.
regoReqToJwt
  :: (MonadRandom m, MonadError e m, AsError e, JE.AsError e)
  => JWK
  -> JwsHeaders
  -> JwsRegisteredClaims
  -> RegistrationRequest
  -> m SignedJWT
regoReqToJwt j h c rr
  = let
      mkCs rc m =
        emptyClaimsSet & setRegisteredClaims rc & unregisteredClaims .~ m
      ssClaims ssreg = mkCs (_ssSigningData ssreg) (ssToAesonClaims ssreg)
      reqAcm = metaDataToAesonClaims . _clientMetaData $ rr
      reqClaims ssb64 =
        mkCs c (reqAcm & at "software_statement" ?~ ssb64)
    in
      do
        -- get the b64 SSA as an aeson Value
        ssb64 <- case _softwareStatement rr of
          EncodedSs ss -> return $ toJSON ss
          DecodedSs ss -> jwtToJson <$> signClaims j (mkHeaders h) (ssClaims ss)
        -- .. and now sign the rego request
        signClaims j (mkHeaders h) (reqClaims ssb64)

--todo add header protection (see makeJWSHeader)
mkHeaders :: JwsHeaders -> JWSHeader ()
mkHeaders h = newJWSHeader ((), _FapiPermittedAlg # _alg h) & kid ?~ HeaderParam
          ()
          (getFapiKid $ _kid h)

setRegisteredClaims :: JwsRegisteredClaims -> ClaimsSet -> ClaimsSet
setRegisteredClaims h claims =
  claims
    &  claimIss
    .~ (getClientIss <$> _iss h)
    &  claimAud
    .~ _aud h
    &  claimIat
    .~ _iat h
    &  claimJti
    .~ (getJTI <$> _jti h)
    &  claimExp
    .~ _exp h

-- | Convert a signed JWT received by the OP into a registration request,
-- verifying the request JWT, and extract the software statement JWT from the
-- claims, and verify that as well.
jwtToRegoReq
  :: (MonadError e m, AsError e, AsJWTError e, JE.AsError e, MonadTime m)
  => (StringOrURI -> Bool)
  -> (StringOrURI -> Bool)
  -> (StringOrURI -> Bool)
  -> (StringOrURI -> Bool)
  -> JWK
  -> SignedJWT
  -> m (JwsHeaders, JwsRegisteredClaims, RegistrationRequest)
jwtToRegoReq audPred issPred audPredSsa issPredSsa jwk jwt = do
  let validationSettings =
        defaultJWTValidationSettings audPred & issuerPredicate .~ issPred
      validationSettingsSsa =
        defaultJWTValidationSettings audPredSsa & issuerPredicate .~ issPredSsa
      c2m c = c ^. unregisteredClaims . to aesonClaimsToMetaData
      fapiAlg = jwt ^? signatures . header . alg . param . _FapiPermittedAlg
      fapiKid = jwt ^? signatures . header . kid . _Just . param . to FapiKid
      jwsHead =
        JwsHeaders
          <$> maybeErrors (_MissingClaim # "alg") fapiAlg
          <*> maybeErrors (_MissingClaim # "kid") fapiKid
  claims <- verifyClaims validationSettings jwk jwt
  ssjwt  <-
    decodeCompact
    =<< BSL.fromStrict
    .   TE.encodeUtf8
    <$> claims
    ^.  unregisteredClaims
    .   to (`getClaim` "software_statement")
  ssclaims <- verifyClaims validationSettingsSsa jwk ssjwt
  -- Get the `software_statement` (ie a JWT), extract the headers and the claims
  ss <- SoftwareStatement <$> getRegisteredClaims ssclaims <*> c2m ssclaims
  -- ... putting that inside the software statement in the RegistrationRequest
  rr <- RegistrationRequest <$> c2m claims <*> pure (DecodedSs ss)
  h <- jwsHead
  c <- getRegisteredClaims claims
  pure $ (h,c,rr)

getRegisteredClaims
  :: (MonadError e m, AsError e, AsJWTError e, JE.AsError e, MonadTime m)
  => ClaimsSet
  -> m JwsRegisteredClaims
getRegisteredClaims claims = do
  i <- getRegClaim claimIss "iss" claims
  a <- getRegClaim claimAud "aud" claims
  t <- getRegClaim claimIat "iat" claims
  j <- getRegClaim claimJti "jti" claims
  e <- getRegClaim claimExp "exp" claims
  return $ JwsRegisteredClaims (Just $ ClientIss i)
                               (Just a)
                               (Just t)
                               (Just e)
                               (Just $ JTI j)
  where getRegClaim g name cs = cs ^. g & maybeErrors (_MissingClaim # name)

aesonClaimsToMetaData
  :: forall e m . (AsError e, MonadError e m) => AesonClaims -> m ClientMetaData
aesonClaimsToMetaData m = do
  _clientName          <- getmClaim m "client_name"
  _clientUri           <- getmClaim m "client_uri"
  _contacts            <- getmClaim m "contacts"
  _logoUri             <- getmClaim m "logo_uri"
  _policyUri           <- getmClaim m "policy_uri"
  _tosUri              <- getmClaim m "tos_uri"
  _subjectType         <- getmClaim m "subject_type"
  _sectorIdentifierUri <- getmClaim m "sector_identifier_uri"
  -- Fail if *both* jwks and jwks_uri are supplied
  _keySet <- join $ liftA2 chkks (getmClaim m "jwks") (getmClaim m "jwks_uri")
  _requestUris         <- getmClaim m "request_uris"
  _redirectUris        <- getClaim m "redirect_uris"
  ra                   <- getmClaim m "request_object_encryption_alg"
  re                   <- getmClaim m "request_object_encryption_enc"
  let _requestObjectEncryption = RequestObjectEncryption <$> ra <*> pure re
  _userinfoSignedResponseAlg <- getmClaim m "userinfo_signed_response_alg"
  ia                         <- getmClaim m "id_token_encrypted_response_alg"
  ie                         <- getmClaim m "id_token_encrypted_response_enc"
  let _idTokenEncryption = IdTokenEncryption <$> ia <*> pure ie
  _responseTypes    <- getmClaim m "response_types"
  _defaultMaxAge    <- getmClaim m "default_max_age"
  _requireAuthTime  <- getmClaim m "require_auth_time"
  _defaultAcrValues <- getmClaim m "default_acr_values"
  _initiateLoginUri <- getmClaim m "initiate_login_uri"
  ua                <- getmClaim m "user_info_encrypted_response_alg"
  ue                <- getmClaim m "user_info_encrypted_response_enc"
  let _userInfoEncryption = UserInfoEncryption <$> ua <*> pure ue
  _idTokenSignedResponseAlg <- getClaim m "id_token_signed_response_alg"
  _requestObjectSigningAlg  <- getClaim m "request_object_signing_alg"
  _grantTypes               <- getmClaim m "grant_types"
  _applicationType          <- getmClaim m "application_type"
  _tokenEndpointAuthMethod  <-
    maybeErrors (_InvalidClaim # "Invalid endpoint auth method.")
    =<< (^? _FapiTokenEndpointAuthMethod)
    <$> getTokEndPtMeth m
  _scope                      <- getmClaim m "scope"
  _softwareId                 <- getmClaim m "software_id"
  _softwareVersion            <- getmClaim m "software_version"
  _clientNotificationEndpoint <- getmClaim m "client_notification_endpoint"
  pure ClientMetaData {..}
 where
  chkks ks u = if isJust ks && isJust u
    then throwError (_InvalidClaim # "jwks xor jwks_uri required.")
    else pure $ JwksVal <$> ks <|> JwksRef <$> u


-- convert a signed jwt to base64 then make it a json Value (for a claim)
jwtToJson :: SignedJWT -> Value
jwtToJson = toJSON . TE.decodeUtf8 . BSL.toStrict . encodeCompact

getClaim
  :: forall e m a
   . (AsError e, MonadError e m, FromJSON a)
  => AesonClaims
  -> T.Text
  -> m a
getClaim m n = m ^. at n & (>>= fromVal n) . maybeErrors (_MissingClaim # n)

getmClaim
  :: forall e m a
   . (AsError e, MonadError e m, FromJSON a)
  => AesonClaims
  -> T.Text
  -> m (Maybe a)
getmClaim m n = maybe (pure Nothing) (n2n) (m ^. at n)
  where
    n2n Null = pure Nothing
    n2n v    = fromVal n v

fromVal
  :: forall e m a . (AsError e, MonadError e m, FromJSON a) => T.Text -> Value -> m a
fromVal n = rToM . fromJSON
 where
  rToM = \case
    Error   s -> throwError (_ParseError # (T.unpack n<>": "<>s))
    Success a -> pure a

maybeErrors :: (AsError e, MonadError e m) => e -> Maybe a -> m a
maybeErrors e = maybe (throwError e) pure

makeLenses ''RegistrationRequest
makeLenses ''SoftwareStatement
makePrisms ''RegoReqSoftwareStatement
makeLenses ''JwsRegisteredClaims
makeLenses ''ClientMetaData
makePrisms ''TokenEndpointAuthMethod
makePrisms ''JwkSet
makeWrapped ''EmailAddress
makeWrapped ''RequestUri
