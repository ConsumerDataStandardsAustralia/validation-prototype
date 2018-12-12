{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Web.ConsumerData.Au.Api.Types.Auth.Registration
  (
      RegistrationRequest(..)
    , JwsRegisteredClaims (..)
    , JwsHeaders (..)
    , ClientMetaData (..)
    -- TODO: Not exporting data constructor for RegoReqSoftwareStatement --- use the smart constructor
    , RegoReqSoftwareStatement (..)
    , SoftwareStatement (..)
    , JTI(..)
    , RegReqAccessToken (..)
    , FapiTokenEndpointAuthMethod (..)
    , Script (..)
    , Language (..)
    , SubjectType (..)
    , EmailAddress (..)
    , ScriptUri (..)
    , RegistrationContacts (..)
    , JwkSet (..)
    , RequestUris (..)
    , FapiEnc (..)
    , FapiResponseTypes (..)
    , FapiApplicationType (..)
    , FapiGrantTypes
    , FapiAcrValues (..)
    , FapiScopes (..)
    , FapiKid (..)
    , X509ThumbPrint (..)
    , HttpsUrl (..)
    , RedirectUrls (..)
    , RequestObjectEncryption (..)
    , IdTokenEncryption (..)
    , DefaultMaxAge (..)
    , UserInfoEncryption (..)
    , MutualTlsSCAT (..)
    , NotificationEndpoint (..)
    , SoftwareId (..)
    , SoftwareVersion (..)
    , fapiGrantTypes
    , GrantType (..)
    , fapiApplicationType
    , ApplicationType (..)
    , TokenEndpointAuthMethod (..)
    , RequestUri (..)
    , redirectUrls
    , aesonClaimsToMetaData
    , metaDataToAesonClaims
    , regoReqToJwt
    , jwtToRegoReq
    , JwksUri (..)
    , fapiTokenEndpointAuthMethod
    , GrantTypes (..)
    , isX5t
    , _X5T
    , _X5T256
    , x509ByteString
    , fapiEnc
    , mutualTlsSCAT
    , registrationErrorType
    , RegistrationErrorDescription (..)
    , RegistrationErrorType (..)
    , RegistrationError (..)
    , ClientIdIssuedAt (..)
    , ClientSecretExpiresAt (..)
    , ClientSecret (..)
    , RegistrationClientUri (..)
    , RegistrationAccessToken (..)
    , RegistrationResponse (..)
    , ClientName (..)
    , ClientDescription (..)
  ) where

import           Control.Applicative                       (liftA2)
import           Control.Lens
    (Lens', Prism', at, makePrisms, prism, prism', to, ( # ), (&), (.~), (?~),
    (^.), (^?), _Right)
import           Control.Monad.Error.Class
    (MonadError, throwError)
import           Control.Monad.Time                        (MonadTime)
import qualified Crypto.JOSE.Error                         as JE
import           Crypto.JOSE.JWA.JWE                       (Enc)
import qualified Crypto.JOSE.JWA.JWE                       as JWE
import           Crypto.JOSE.JWK                           (JWK)
import           Crypto.JOSE.JWS
    (HeaderParam (..), kid, newJWSHeader, x5t, x5tS256)
import           Crypto.JOSE.Types
    (Base64SHA1 (..), Base64SHA256 (..))
import           Crypto.JWT
    (AsJWTError, Audience, ClaimsSet, NumericDate, SignedJWT, StringOrURI,
    claimAud, claimExp, claimIat, claimIss, claimJti, decodeCompact,
    defaultJWTValidationSettings, emptyClaimsSet, encodeCompact,
    issuerPredicate, signClaims, unregisteredClaims, verifyClaims)
import           Crypto.Random.Types                       (MonadRandom)
import           Data.Aeson
    (FromJSON (..), Result (..), ToJSON (..), Value, fromJSON)
import           Data.Bool                                 (bool)
import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Lazy                      as BSL
    (fromStrict, toStrict)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as M
import           Data.Set                                  (Set, isSubsetOf)
import qualified Data.Set                                  as Set
import qualified Data.Text                                 as T
import           Data.Text.Encoding                        as TE
    (decodeUtf8, encodeUtf8)
import           GHC.Generics                              (Generic)
import           Prelude                                   hiding (exp)
import           Text.URI                                  (URI, mkURI)
import qualified Text.URI                                  as URI
import           Text.URI.Lens
    (authHost, uriAuthority, uriScheme)
import           Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientId, ClientIss (..), FapiPermittedAlg, RedirectUri, ResponseType,
    Scopes, getRedirectUri, _FapiPermittedAlg)
import           Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsError, _MissingClaim, _ParseError)

-- | The client registration endpoint is an OAuth 2.0 endpoint that is designed to allow a client to be dynamically registered with the authorization server. 'RegistrationRequest' represents a client request for registration containing meta-data elements specified in <https://tools.ietf.org/html/rfc7591 §RFC7591 - OAuth 2.0 Dynamic Client Registration Protocol> and <https://openid.net/specs/openid-connect-registration-1_0.html §OIDC registration>. Each request must contain a software statement assertion (a JWT is issued and signed by the OpenBanking Directory). Metadata values may be duplicated in the registration request, but if different, those in the software statement will take precedence and override those in the request.
--
--A RP will submit the registration request to a OP in order to receive `client_id` credentials. The registration request parameters are sent in a JWT signed by the RP [UK OB mandates this] and include the signed software statement assertion as a JWT claim.
--
-- Full details of CDR dynamic client registration can be found in the <https://consumerdatastandardsaustralia.github.io/infosec/#discovery-and-registration §CDR infosec standards>.

--   * TODO: exclude exports of data constructors for those that have SC's
--   * TODO: move common data types into Common: FapiJwtHeaders etc
--   * TODO: implement predicate checking (aud/iss) for the registration request and the software statement.
--   * TODO: @Script@ type does not yet support any BCP47 [RFC5646] language tags.
--   * TODO: JSON encoder/decoder must support BCP47 [RFC5646] language tags in keys.
--   * TODO: Implementation of ToJSON/FromJSON to correctly map to OIDC expected values.
--   * TODO: Successful registration response will return with a HTTP 201.
--   * TODO: data validation, generation and persistence of server response data.
--   * TODO: JWKS SSA validation based on URI
--   * TODO: /clients endpoint

-- | Determines the set of credentials that will be used by a client when accessing /token endpoint. OIDC requires a restricted subset of the allowed OAuth values (<https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>). Also, because OZ OB only supports `code id_token` and `code id_token token` (i.e. the `hybrid` flow), this means `grant_types` must contain at least `authorization_code` and `implicit`, as per the OIDC Registration spec (<https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>).
-- NB: For UK OB, `client_credentials` is required as a grant type, as is it needed for the client to submit a JWT to the /token endpoint to obtain a consent ID / payment ID before sending the request to /payments.
data GrantType = Implicit | AuthorizationCode | RefreshToken -- ClientCredentials
  deriving (Generic, ToJSON, FromJSON, Show, Eq, Ord)

newtype GrantTypes = GrantTypes (Set GrantType)
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype FapiGrantTypes = FapiGrantTypes GrantTypes
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | The X.509 Certificate Thumbprint (SHA-1) field (@x5t@) must be included in the headers if present on the JWK, as must the @x5t#S256@ header (SHA-256). See <https://consumerdatastandardsaustralia.github.io/infosec/#jose-jwt-header §CDR spec>, <https://tools.ietf.org/html/rfc7515#section-4.1.7 §RFC7515 4.1.7> has further details.
-- TODO: check if we can borrow Jose types
data X509ThumbPrint = X5T ByteString | X5T256 ByteString
  deriving (Generic, Show, Eq)

-- TODO: NB: this may be the used instead of the KID
_X5T :: Prism' X509ThumbPrint ByteString
_X5T = prism' X5T
         (\case
             X5T a -> Just a
             _ -> Nothing
         )

_X5T256 :: Prism' X509ThumbPrint ByteString
_X5T256 = prism' X5T256
         (\case
             X5T256 a -> Just a
             _ -> Nothing
         )

isX5t :: Lens' X509ThumbPrint Bool
isX5t f o@(X5T a)    = fmap (bool (X5T256 a) o) (f True)
isX5t f o@(X5T256 a) = fmap (bool o (X5T a)) (f False)

x509ByteString :: Lens' X509ThumbPrint ByteString
x509ByteString f (X5T a)    = fmap X5T (f a)
x509ByteString f (X5T256 a) = fmap X5T256 (f a)

--TODO: remove all to/fromJSONs
instance ToJSON X509ThumbPrint where
  toJSON  = undefined

instance FromJSON X509ThumbPrint where
  parseJSON = undefined

-- | Smart constructor for producing FAPI permitted `grant_types`
-- TODO: Investigate further how SCs will work with testing, consider type parameters (fapi, cdr, testing possibities)
fapiGrantTypes :: GrantTypes -> Maybe FapiGrantTypes
fapiGrantTypes (GrantTypes grantTypes) =  if grantTypes `isSubsetOf` permittedGrantTypes && requiredGrantTypes `isSubsetOf` grantTypes then Just . FapiGrantTypes . GrantTypes $ grantTypes else Nothing where
  permittedGrantTypes = Set.fromList [Implicit,AuthorizationCode,RefreshToken]
  requiredGrantTypes = Set.fromList [Implicit,AuthorizationCode]

data ApplicationType = Web | Native
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
newtype FapiApplicationType = FapiApplicationType ApplicationType
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Smart constructor for producing FAPI permitted @application_type@s. In the current version of CDR, only @web@ is allowed.
fapiApplicationType :: ApplicationType -> Maybe FapiApplicationType
fapiApplicationType a = case a of
                           Web -> Just . FapiApplicationType $ Web
                           _   -> Nothing

-- TODO: Use an email type.
newtype RegistrationContacts = RegistrationContacts [EmailAddress]
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype EmailAddress = EmailAddress T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: discuss with Luke if this can be excluded and mentioned in the delta
-- | Text with support for BCP47 [RFC5646] language tags in keys.
-- e.g. client_name#ja-Jpan-JP :: クライアント名", as required by https://openid.net/specs/openid-connect-registration-1_0.html#LanguagesAndScripts.
data Script = Script Language T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
data Language = DefaultLang
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
data ScriptUri = ScriptUri Language URI
  deriving (Generic, Show, Eq)

instance ToJSON ScriptUri where
  toJSON (ScriptUri _ uri) =
    toJSON $ URI.render uri

instance FromJSON ScriptUri where
  parseJSON =
    fmap (ScriptUri DefaultLang) . (>>= toParser) . fmap mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

-- TODO: create smart constructor
newtype HttpsUrl = HttpsUrl URI
  deriving (Generic, Show, Eq)

instance ToJSON HttpsUrl where
  toJSON (HttpsUrl uri) =
    toJSON $ URI.render uri

instance FromJSON HttpsUrl where
  parseJSON =
    fmap HttpsUrl . (>>= toParser) . fmap mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

-- @subject_type@ requested for responses to the client; only @pairwise@ is supported in <https://consumerdatastandardsaustralia.github.io/infosec/#data-holder-metadata §CDR>
data SubjectType = Pairwise -- `Public` type not supported
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype JwksUri = JwksUri URI
  deriving (Generic, Show, Eq)

instance ToJSON JwksUri where
  toJSON (JwksUri uri) =
    toJSON $ URI.render uri

instance FromJSON JwksUri where
  parseJSON =
    fmap JwksUri . (>>= toParser) . fmap mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

--TODO use keyset instead of Text for JwksVal
--TODO must include @use@ to specify either enc or sig
--TODO must include kid
-- | Client's JSON Web Key Set
data JwkSet = JwksRef JwksUri --  ^ @jwks_uri@ parameter that specifies a URL for the client's JSON Web Key Set for pass-by-reference
  | JwksVal T.Text -- ^ @jwks@ parameter that specifies client's JSON Web Key Set document (passed by value). It is recommended [OIDC-R] that keys are passed by reference if possible.
  deriving (Generic, FromJSON, ToJSON, Show, Eq)
makePrisms ''JwkSet

-- | JWE @alg@ (and optional @enc@) algorithms for encrypting the ID token issued to the client (server must comply). @none@ is not permitted, unless the client only uses response types that return no ID token from the authorization endpoint (such as when only using the authorization code flow). This is not currently an option in OB.
--
-- Default @alg@ is @PS256@. If only the @id_token_encrypted_response_alg@ is specified, the default for @enc@ is @A128CBC-HS256@.
data IdTokenEncryption = IdTokenEncryption
  {
    idTokenAlg :: FapiPermittedAlg
  , idTokenEnc :: FapiEnc
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | JWE @alg@ (and optional @enc@) algorithms for encrypting the UserInfo response sent to the client (server must comply). @none@ is not permitted, unless the client only uses response types that return no ID token from the authorization endpoint (such as when only using the authorization code flow). This is not currently an option in OB.
--
-- Default @alg@ is @PS256@. If only the @id_token_encrypted_response_alg@ is specified, the default for @enc@ is @A128CBC-HS256@.
data UserInfoEncryption = UserInfoEncryption
  {
    userInfoAlg :: FapiPermittedAlg
  , userInfoEnc :: Maybe FapiEnc
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data RequestObjectEncryption = RequestObjectEncryption
  {
    reqObjAlg :: FapiPermittedAlg
  , reqObjEnc :: Maybe FapiEnc
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data TokenEndpointAuthMethod = ClientSecretPost | ClientSecretBasic | ClientSecretJwt FapiPermittedAlg | PrivateKeyJwt FapiPermittedAlg | TlsClientAuth TlsClientAuthSubjectDn | None
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | A string representation of the expected subject distinguished name of the certificate the OAuth client will use in mutual TLS authentication.
newtype TlsClientAuthSubjectDn = TlsClientAuthSubjectDn T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Only PrivateKeyJwt and TlsClientAuth are supported by CDR. @token_endpoint_auth_signing_alg@ is required if using @PrivateKeyJwt@ @token_endpoint_auth_method@, and @tls_client_auth_subject_dn@ must be supplied if using @tls_client_auth@ (as per <https://consumerdatastandardsaustralia.github.io/infosec/#recipient-client-registration §CDR Registration>). All token requests will be rejected by the server if they are not signed by the algorithm specified in @alg@, or if they are signed with @none@, or if the subject distinguished name of the certificate does not match that of the MTLS certificate.
newtype FapiTokenEndpointAuthMethod = FapiTokenEndpointAuthMethod TokenEndpointAuthMethod
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

fapiTokenEndpointAuthMethod :: TokenEndpointAuthMethod -> Maybe FapiTokenEndpointAuthMethod
fapiTokenEndpointAuthMethod = (^?_FapiTokenEndpointAuthMethod)

_FapiTokenEndpointAuthMethod :: Prism' TokenEndpointAuthMethod FapiTokenEndpointAuthMethod
_FapiTokenEndpointAuthMethod = prism (\case
          FapiTokenEndpointAuthMethod f -> f
      )
      (\case
          -- ClientSecretJwt f -> Right . FapiTokenEndpointAuthMethod . ClientSecretJwt $ f -- Not supported in CDR.
          PrivateKeyJwt f -> Right . FapiTokenEndpointAuthMethod . PrivateKeyJwt $ f
          TlsClientAuth f -> Right . FapiTokenEndpointAuthMethod . TlsClientAuth $ f
          e -> Left e
      )

-- | FAPI accepted algorithms for content encryption, based on <https://tools.ietf.org/html/rfc7518 §RFC 7518 5. Cryptographic Algorithms for Content Encryption>.
data FapiEnc =
      A128CBC_HS256
    | A192CBC_HS384
    | A256CBC_HS512
    | A128GCM
    | A192GCM
    | A256GCM
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: this doesn't look right
fapiEnc :: Prism' Enc FapiEnc
fapiEnc =
   prism' (\case
            A128CBC_HS256 -> JWE.A128CBC_HS256 -- Unsure if the CBC ciphers are acceptable
            A192CBC_HS384 -> JWE.A192CBC_HS384
            A256CBC_HS512 -> JWE.A256CBC_HS512
            A128GCM -> JWE.A128GCM
            A192GCM -> JWE.A192GCM
            A256GCM -> JWE.A256GCM
         )
         (\case
            JWE.A128CBC_HS256 -> Just A128CBC_HS256
            JWE.A192CBC_HS384->  Just A192CBC_HS384
            JWE.A256CBC_HS512-> Just   A256CBC_HS512
            JWE.A128GCM -> Just   A128GCM
            JWE.A192GCM -> Just  A192GCM
            JWE.A256GCM -> Just  A256GCM
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

--TODO iso
-- _KidValue :: Prism' Value FapiKid
-- _KidValue =
--    prism (\case
--              FapiKid v -> A.String v
--          )
--          (\case
--              A.String v -> Right . FapiKid $ v
--              e -> Left e
--          )

newtype RequestUris = RequestUris {
  getRequestUris  :: [RequestUri]}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype RequestUri =
  RequestUri {getRequestUri :: URI}
  deriving (Show, Eq)

instance ToJSON RequestUri where
  toJSON (RequestUri uri) =
    toJSON $ URI.render uri

instance FromJSON RequestUri where
  parseJSON =
    fmap RequestUri . (>>= toParser) . fmap URI.mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

-- | 'RedirectUrls' is a non-empty array of redirection URI values used by the client to match against supplied @redirect_uri@ request parameter. If using @web@ scheme (as per CDR) these must all be HTTPS, and must not use 'localhost' as hostname.
newtype RedirectUrls = RedirectUrls {
  getRedirectUrls  :: [RedirectUri]}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Constructor for @redirect_url@ array; all URLs must be HTTPS, none may be localhost, as mandated by CDR.
redirectUrls :: [RedirectUri] -> Maybe RedirectUrls
redirectUrls uris = if not (null uris) && allValid uris then Just . RedirectUrls $ uris else Nothing where
  isValidHost uri = and $ liftA2 (/=) (URI.mkHost "localhost") (uri ^? uriAuthority . _Right . authHost)
  isHttps uri = and $ liftA2 (==) (URI.mkScheme "https") (uri ^. uriScheme)
  allValid = all (liftA2 (&&) isValidHost isHttps . getRedirectUri)

-- TODO: It is unclear that our ResponseType type already has a smart constructor; perhaps it should be renamed with FAPI prefix?
-- | FAPI acceptable values are either @code id_token@ or @code id_token token@.
newtype FapiResponseTypes = FapiResponseTypes [ResponseType]
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype FapiScopes = FapiScopes Scopes
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

--TODO: use Auth.Common's values here: must contain @urn:cds.au:cdr:3@ at a minimum.
newtype FapiAcrValues = FapiAcrValues T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype MutualTlsSCAT = MutualTlsSCAT Bool
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Smart constructor for @mutual_tls_sender_constrained_access_tokens@, which can only have the value of True.
mutualTlsSCAT :: MutualTlsSCAT
mutualTlsSCAT = MutualTlsSCAT True

newtype NotificationEndpoint = NotificationEndpoint HttpsUrl
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data RegistrationRequest = RegistrationRequest {
    _regoReqJwtHeaders       :: JwsHeaders
  , _regoReqRegClaims        :: JwsRegisteredClaims
  , _regReqClientMetaData    :: ClientMetaData
  , _regReqsoftwareStatement :: RegoReqSoftwareStatement -- ^ A signed JWT containing metadata about the client software. RFC7591 mandates that this is a JWS.
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

--TODO better type only allowing {YYYY-MM-DD | YYYY-MM-DD.<V>} format.
-- | This key ID must be the value of the date on which the key was published in the format YYYY-MM-DD and must be unique within the set. If more than one key is published on the same date, the kid must consist of the following format YYYY-MM-DD.<V> where <V> represents an increasing version number and positive integer. See the <https://consumerdatastandardsaustralia.github.io/infosec/#json-web-key-sets §infosec spec> for more info.
newtype FapiKid = FapiKid {
  getFapiKid :: T.Text }
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
-- makeWrapped ''FapiKid

-- | These claims are JWS registered claims required for the registration requests.
data JwsRegisteredClaims = JwsRegisteredClaims
  {
    _iss :: Maybe ClientIss  -- ^ The name of the RP. RFC7591 2.3 mandates issuer claim is present if supplied in a software statement.
  , _aud :: Maybe Audience -- ^ The OP.
  , _iat :: Maybe NumericDate -- ^ Issued at.
  , _exp :: Maybe NumericDate -- ^ Expiration time.
  , _jti :: Maybe JTI -- ^ JWT ID.
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

data JwsHeaders = JwsHeaders
  {
     _alg     :: FapiPermittedAlg -- ^ The JWK @alg@ to sign the request with.
    ,_kid     :: FapiKid -- ^ @kid@ is a required header for <https://consumerdatastandardsaustralia.github.io/infosec/#jose-jwt-header §CDR>.
    , _thumbs :: Maybe X509ThumbPrint -- ^ If present on the JWK, the X509 thumbprint must be included in the header.
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | The following claims are specified in <https://tools.ietf.org/html/rfc7591 §RFC7591: 3.1 Client Registration Request> to be used in both the registration request object and the software statement, all of them as optional. However, some fields are non-optional under FAPI, and even fewer are optional under <https://consumerdatastandardsaustralia.github.io/infosec/#recipient-client-registration §CDR>
data ClientMetaData = ClientMetaData {
    _requestObjectSigningAlg                :: FapiPermittedAlg  -- The `alg` algorithm that must be used for signing request objects sent to the OP.
  , _applicationType                        :: FapiApplicationType -- ^ Kind of the application. CDR mandates this to be just @web@ (i.e no @native@ apps are allowed).
  , _tokenEndpointAuthMethod                :: FapiTokenEndpointAuthMethod -- ^ Specifies which token endpoint authentication method the client will use, and also the algorithm (@token_endpoint_auth_signing_alg@) that must be used for signing if a JWT auth method is used.
  , _grantTypes                             :: Maybe FapiGrantTypes -- ^ The set of grant types that a client will restrict itself to using (see <https://openid.net/specs/openid-connect-registration-1_0.html OIDC-R 2. Client Metadata> and <https://tools.ietf.org/html/rfc7591 §RFC7591 2. Client Metadata>). If omitted, the default is that the client will use only the `authorization_code` grant type.
  , _clientName                             :: Script -- ^ Human-readable name of the client to be presented to the end user. Mandatory field according to CDR.
  , _clientUri                              :: Maybe ScriptUri -- ^ URL of the home page of the client.
  , _contacts                               :: Maybe RegistrationContacts  -- ^ Array of e-mail addresses of people responsible for the client.
  , _logoUri                                :: Maybe ScriptUri -- ^ URL that references a logo for the client application.
  , _policyUri                              :: Maybe ScriptUri -- ^ URL that the client provides to the end user in order to read about the how the user data will be used.
  , _tosUri                                 :: Maybe ScriptUri -- ^ URL that the client provides to the end user to read about the client's terms of service.
  , _subjectType                            :: SubjectType -- ^ Requested for responses to this client. Mandatory field according to CDR.
  , _sectorIdentifierUri                    :: Maybe HttpsUrl -- ^ References an HTTPS URL of a remote file containing a single JSON array of @redirect_uri@ values.
  , _keySet                                 :: JwkSet -- ^ Either the @jwks_uri@ or @jwks@ parameter specifying client's JWKS.
  , _requestUris                            :: Maybe RequestUris -- ^ Array of @request_uri@ values that are pre-registered by the RP for use at the OP, which may cache their contents and not retrieve them at the time they are used in a request.
  , _redirectUris                           :: RedirectUrls -- ^ Non-empty array of redirection URI values used by the client to match against supplied @redirect_uri@ request parameter. If using @web@ scheme these must all be HTTPS, and must not use 'localhost' as hostname. Mandatory field, according to OIDC-R. FAPI restricts these to all be HTTPS.
  , _requestObjectEncryption                :: Maybe RequestObjectEncryption -- ^ The @request_object_encryption_alg@ and @request_object_encryption_enc@ parameters for specifying the JWS `alg` and `enc` algorithms (that might be) used when encrypting request objects. If both signed and encrypted, signing will occur first, and then encryption, with the result being a nested JWT. Warning: a RP can supply unencrypted requests, even if this is present, as this is only a declaration that the RP 'might' encrypt request objects. See 'RequestObjectEncryption' for more info.
  , _userinfoSignedResponseAlg              :: Maybe FapiPermittedAlg -- ^ The @userinfo_signed_response_alg@ parameter for specifying which JWS `alg` algorithm should be used for signing UserInfo responses.
  , _idTokenEncryption                      :: IdTokenEncryption -- ^ The @id_token_encrypted_response_alg@ and @id_token_encrypted_response_enc@ values for specifying how the ID token should be encrypted; see 'IdTokenEncryption' for more information. Mandatory field according to CDR.
  , _responseTypes                          :: Maybe FapiResponseTypes -- ^ JSON array containing a list of OAuth 2.0 @response_type@ values that the client will restrict itself to using; defaults to @code id_token@ if omitted. FAPI restricts these to be either `code id_token` or `code id_token token`.
  , _defaultMaxAge                          :: Maybe DefaultMaxAge -- ^ Specifies that the end user must be actively authenticated if the end user was authenticated longer than the specified number of seconds ago; the @max_age@ request object parameter overides this.
  , _requireAuthTime                        :: Maybe Bool -- ^ Whether the @auth_time@ claim in the ID token is required, defaults to False.
  , _defaultAcrValues                       :: Maybe FapiAcrValues -- ^ Default 'Authentication Context Class Reference' (ACR) values for requests, values ordered by preference; defaults overridden by supplied values.
  , _initiateLoginUri                       :: Maybe HttpsUrl -- ^ HTTS URL which can be used by a third-party to initiate a login by the RP.
  , _userInfoEncryption                     :: Maybe UserInfoEncryption -- ^ The @userinfo_encrypted_response_alg@ and @userinfo_encrypted_response_enc@ values for specifying how UserInfo response should be encrypted; see 'UserInfoEncryption' for more information.
  , _idTokenSignedResponseAlg               :: FapiPermittedAlg  -- ^ The `alg` for signing the ID token issued to this client. NB: OIDC-R allows this to be optional (see <https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>), but by using FAPI the implication is that it is mandatory (<https://openid.net/specs/openid-financial-api-part-2.html#public-client §FAPI RW - Section 5.2.3 >). Mandatory field according to CDR.
  , _scope                                  :: Maybe FapiScopes -- ^ A set of scopes, containing at least @openid@.
  , _softwareId                             :: Maybe SoftwareId -- ^ Unique identifier string for the client, which should remain the same across all instances of the client software, and all versions of the client software. This must match the software ID in the SSA if supplied.
  , _softwareVersion                        :: Maybe SoftwareVersion  -- ^ The version number of the software should a TPP choose to register and / or maintain it.
  , _mutualTlsSenderConstrainedAccessTokens :: MutualTlsSCAT -- ^ @mutual_tls_sender_constrained_access_tokens@ indicating the client's intention to use mutual TLS sender constrained access tokens; CDR required field that must always be `true`.
  , _clientNotificationEndpoint             :: NotificationEndpoint -- ^ @client_notification_endpoint@ - CDR required URI for CIBA callback.
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: Add smart constructor to only allow supply of encoded SS, depending on the CDR spec
data RegoReqSoftwareStatement = EncodedSs T.Text | DecodedSs SoftwareStatement
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data SoftwareStatement = SoftwareStatement {
    _ssSigningData :: JwsRegisteredClaims
  , _ssMetaData    :: ClientMetaData -- ^ All the claims to include in the SSA; RFC7591 allows any/all that are included in the request object, however SSA claims take precedence over request object claims (3.1.1).
}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | The folowing fields are specified by UK OB. See <https://openbanking.atlassian.net/wiki/spaces/DZ/pages/36667724/The+OpenBanking+OpenID+Dynamic+Client+Registration+Specification+-+v1.0.0-rc2 §UK OB Spec> for more information) (NB: the OB naming convention deviates from OIDC-R/RFC7591 in that it includes a `software_` prefix in the field keys.)
  -- , _clientId            :: Maybe ClientId -- ^ The Client ID Registered at OB used to access OB resources.
  -- , _clientDescription   :: Maybe ClientDescription -- ^ Human-readable detailed description of the client.
  -- , _environment         :: Maybe T.Text -- ^  Requested additional field to avoid certificate check. This field is not specified anywhere other than UK OB.
  -- , _mode                :: Maybe SoftwareMode -- ^ ASPSP requested additional field to indicate that this software is "Test" or "Live" the default is "Live". Impact and support for "Test" software is up to the ASPSP. This field is not specified anywhere other than UK OB.
  -- , _onBehalfOfOrg       :: Maybe URI -- ^ A reference to fourth party organsiation resource on the OB Directory if the registering TPP is acting on behalf of another. This field is not specified anywhere other than UK OB.
  -- , _onBehalfOfOrgType   :: Maybe T.Text -- ^ The type of organisaion that this software has a relationship with. Regulated on OB Directory, Not regulated, Regulated not On OB Directory. This field is not specified anywhere other than UK OB.
  -- , _roles               :: Maybe SoftwareRoles -- ^ A multi-value list of PSD2 roles that this software is authorized to perform e.g "PISP", "AISP". This field is not specified anywhere other than UK OB.
  -- , _organisationCompetentAuthorityClaims :: Maybe OrgCompetentAuthClaims -- ^ Authorisations granted to the organsiation by an NCA.
  -- , _orgStatus                            :: Maybe ClientStatus  -- ^ Included to cater for voluntary withdrawal from OB scenarios: Active, Revoked or Withdrawn
  -- , _orgId                                :: Maybe OrgId -- ^ The Unique TPP or ASPSP ID held by OpenBanking.
  -- , _orgName                              :: Maybe OrgName  -- ^  Legal Entity Identifier or other known organisation name.
  -- , _orgContacts                          :: Maybe OrgContacts -- ^  JSON array of objects containing a triplet of name, email, and phone number.
  -- , _orgJwksEndpoint                      :: Maybe JwksUri  -- ^ Contains all active signing and network certs for the organisation.
  -- , _orgJwksRevokedEndpoint               :: Maybe JwksUri -- ^ Contains all revoked signing and network certs for the organisation.
  -- , _obRegistryTos                        :: Maybe URI -- ^ A link to the OB registries terms of service page.
  -- , _jwksRevokedEndpoint            :: Maybe HttpsUrl -- ^ Contains all revoked signing and network certs for the software.
 -- }

-- data OrgCompetentAuthClaims = OrgCompetentAuthClaims {
--     _authorityId    :: Maybe T.Text
--   , _registrationId :: Maybe T.Text
--   , _status         :: Maybe T.Text
--   , _authorisations :: Maybe AuthorityAuths
-- }

-- data AuthorityAuths = AuthorityAuths {
--     _memberState :: Maybe T.Text
--   , _roles       :: Maybe T.Text
-- }

-- UK OB implements contacts with names and phones alongside the (typically standard) email-address
-- data OrgContacts = OrgContacts {
--     _name  :: Maybe T.Text
--   , _email :: Maybe EmailAddress
--   , _phone :: Maybe T.Text
-- }
-- newtype OrganisationId = OrganisationId T.Text
-- newtype OrgId = OrgId T.Text
-- newtype OrgName = OrgName T.Text
-- newtype SoftwareRoles = SoftwareRoles T.Text
-- newtype SoftwareRoles = SoftwareRoles T.Text
-- data ClientStatus = Active | Revoked | Withdrawn

newtype SoftwareId = SoftwareId T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
newtype ClientDescription = ClientDescription T.Text
newtype ClientName = ClientName T.Text
newtype SoftwareVersion = SoftwareVersion T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Response type for a dynamic registration request. NB: A successful response will return a HTTP 201 (<https://openid.net/specs/openid-connect-registration-1_0.html §3.2. Client Registration Response>)
data RegistrationResponse = RegistrationResponse
  {
    _clientId                 :: ClientId -- ^ OAuth 2.0 unique client identifier string. RFC7591 mandated.
  , _clientSecret             :: Maybe ClientSecret -- ^ OAuth 2.0 unique client secret string, used by confidential clients to authenticate to token end-point. NB: This will only ever be used by the `client_secret_jwt` client auth method.
  , _clientIdIssuedAt         :: Maybe ClientIdIssuedAt -- ^ Time at which the client identifier was issued.
  , _regReqAccessToken        :: Maybe RegReqAccessToken -- ^ URI and token to perform further operations if necessary.
  , _regRespClientMetaData    :: ClientMetaData -- ^ The rego server must return all registered metadata about the client (OIDC-R 3.2. Client Registration Response).
  , _regRespSoftwareStatement :: SoftwareStatement -- ^ RFC7591 3.2.1 mandates software statement supplied in a request must be returned unmodified
}

-- | Registration access token and location of URI that can be used by the client to perform further operations upon the client registration.
data RegReqAccessToken = RegReqAccessToken RegistrationAccessToken RegistrationClientUri
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

data RegistrationErrorType = INVALID_REDIRECT_URI -- ^  The value of one or more redirection URIs is invalid.
                           | INVALID_CLIENT_METADATA -- ^ The value of one of the client metadata fields is invalid and the server has rejected this request.
                           | INVALID_SOFTWARE_STATEMENT -- ^ The software statement presented is invalid.
                           | UNAPPROVED_SOFTWARE_STATEMENT -- ^ The software statement presented is not approved for use by this authorization server.

newtype RegistrationErrorDescription = RegistrationErrorDescription T.Text


registrationErrorType ::
  Prism' T.Text RegistrationErrorType
registrationErrorType =
  prism (\case
            INVALID_REDIRECT_URI -> "invalid_redirect_uri"
            INVALID_CLIENT_METADATA -> "invalid_client_metadata"
            INVALID_SOFTWARE_STATEMENT -> "invalid_software_statement"
            UNAPPROVED_SOFTWARE_STATEMENT -> "unapproved_software_statement"
        )
        (\case
            "invalid_redirect_uri" -> Right INVALID_REDIRECT_URI
            "invalid_client_metadata" -> Right INVALID_CLIENT_METADATA
            "invalid_software_statement" -> Right INVALID_SOFTWARE_STATEMENT
            "unapproved_software_statement" -> Right UNAPPROVED_SOFTWARE_STATEMENT
            e -> Left e
        )

type AesonClaims = HashMap T.Text Value

-- Jose needs JWT claims to be supplied as Aeson Values
metaDataToAesonClaims ::
  ClientMetaData
  -> AesonClaims
metaDataToAesonClaims ClientMetaData{..} =
  M.empty
    & at "client_name"?~ toJSON _clientName
    & at "client_uri".~ (toJSON <$> _clientUri)
    & at "contacts".~ (toJSON <$> _contacts)
    & at "logo_uri".~ (toJSON <$> _logoUri)
    & at "policy_uri".~ (toJSON <$> _policyUri)
    & at "tos_uri".~ (toJSON <$> _tosUri)
    & at "subject_type"?~ toJSON _subjectType
    & at "sector_identifier_uri".~ (toJSON <$> _sectorIdentifierUri)
    -- TODO: The spec on this is going to change, awaiting.
    -- Either the jwks or the jwks_uri must be supplied.
    -- & at "jwks".~ (toJSON <$> _keySet)
    -- & at "jwks_uri".~ (toJSON <$> _keySet)
    & at "request_uris".~ (toJSON <$> _requestUris)
    & at "redirect_uris" ?~ toJSON _redirectUris
    & at "request_object_encryption_alg".~ (toJSON  . reqObjAlg <$> _requestObjectEncryption)
    & at "request_object_encryption_enc".~ (toJSON  <$> (reqObjEnc =<< _requestObjectEncryption))
    & at "userinfo_signed_response_alg".~ (toJSON <$> _userinfoSignedResponseAlg)
    -- TODO: The spec on this is going to change, awaiting.
    -- & at "id_token_encrypted_response_alg".~ (toJSON . idTokenAlg <$> _idTokenEncryption)
    -- & at "id_token_encrypted_response_enc".~ (toJSON <$> (idTokenEnc =<< _idTokenEncryption))
    & at "response_types".~ (toJSON <$> _responseTypes)
    & at "default_max_age".~ (toJSON <$> _defaultMaxAge)
    & at "require_auth_time".~ (toJSON <$> _requireAuthTime)
    & at "default_acr_values".~ (toJSON <$> _defaultAcrValues)
    & at "initiate_login_uri".~ (toJSON <$> _initiateLoginUri)
    & at "user_info_encrypted_response_alg".~ (toJSON . userInfoAlg <$> _userInfoEncryption)
    & at "user_info_encrypted_response_enc".~ (toJSON <$> (userInfoEnc =<< _userInfoEncryption))
    & at "id_token_signed_response_alg"?~ toJSON _idTokenSignedResponseAlg
    & at "request_object_signing_alg"?~ toJSON _requestObjectSigningAlg
    & at "grant_types" .~ (toJSON <$> _grantTypes)
    & at "application_type"?~ toJSON _applicationType
    & at "token_endpoint_auth_method"?~ toJSON _tokenEndpointAuthMethod
    & at "scope".~ (toJSON <$> _scope)
    & at "software_id".~ (toJSON <$> _softwareId)
    & at "software_version".~ (toJSON <$> _softwareVersion)

-- | Currently all meta-data is included in the software statement.
ssToAesonClaims ::
  SoftwareStatement
  -> AesonClaims
ssToAesonClaims = metaDataToAesonClaims . _ssMetaData

-- | Sign a registration request for sending to OP.
regoReqToJwt ::
  ( MonadRandom m
  , MonadError e m
  , AsError e
  , JE.AsError e
  )
  => JWK
  -> RegistrationRequest
  -> m SignedJWT
regoReqToJwt jwk rr =
  let
    mkCs h m = emptyClaimsSet & setRegisteredClaims h & unregisteredClaims .~ m
    ssClaims ssreg = mkCs (_ssSigningData ssreg) (ssToAesonClaims ssreg)
    reqAcm = metaDataToAesonClaims . _regReqClientMetaData $ rr
    reqClaims ssb64 = mkCs (_regoReqRegClaims rr) (reqAcm & at "software_statement" ?~ ssb64)
    rrh = _regoReqJwtHeaders rr
    jwsHead = newJWSHeader ((), _FapiPermittedAlg # _alg rrh)
              & kid ?~ HeaderParam () (getFapiKid $ _kid rrh)
              & x5t .~ (HeaderParam () . Base64SHA1 . (^. _X5T) <$> _thumbs rrh)
              & x5tS256 .~ (HeaderParam () . Base64SHA256 . (^. _X5T256) <$> _thumbs rrh)
  in do
    -- get the b64 SSA as an aeson Value
    ssb64 <- case _regReqsoftwareStatement rr of
                  EncodedSs ss -> return $ toJSON ss
                  DecodedSs ss -> jwtToJson <$> signClaims jwk jwsHead (ssClaims ss)
    -- .. and now sign the rego request
    signClaims jwk jwsHead (reqClaims ssb64)

setRegisteredClaims :: JwsRegisteredClaims -> ClaimsSet -> ClaimsSet
setRegisteredClaims h claims = claims
      & claimIss .~ (getClientIss <$>_iss h)
      & claimAud .~ _aud h
      & claimIat .~ _iat h
      & claimJti .~ (getJTI <$> _jti h)
      & claimExp .~ _exp h

-- | Convert a signed JWT received by the OP into a registration request, verifying the request JWT, and extract the software statement JWT from the claims, and verify that as well.
jwtToRegoReq  ::
  ( MonadError e m
  , AsError e
  , AsJWTError e
  , JE.AsError e
  , MonadTime m
  )
  => (StringOrURI -> Bool)
  -> (StringOrURI -> Bool)
  -> JWK
  -> SignedJWT
  -> m RegistrationRequest
jwtToRegoReq audPred issPred jwk jwt = do
  let
    -- TODO: seperate predicates here for the JWT?
    validationSettings = defaultJWTValidationSettings audPred & issuerPredicate .~ issPred
    c2m c = c ^. unregisteredClaims . to aesonClaimsToMetaData
    jwsHead = undefined
  claims <- verifyClaims validationSettings jwk jwt
  ssjwt <- decodeCompact =<< BSL.fromStrict . TE.encodeUtf8 <$> claims ^. unregisteredClaims . to (`getClaim` "software_statement")
  ssclaims <- verifyClaims validationSettings jwk ssjwt
  -- Get the `software_statement` (ie a JWT), extract the headers and the claims
  ss <-  SoftwareStatement <$> getRegisteredClaims ssclaims <*> c2m ssclaims
  -- ... putting that inside the software statement in the RegistrationRequest
  RegistrationRequest <$> jwsHead <*> getRegisteredClaims claims <*>  c2m claims <*> pure (DecodedSs ss)

getRegisteredClaims :: (  MonadError e m
                    , AsError e
                    , AsJWTError e
                    , JE.AsError e
                    , MonadTime m
                    ) => ClaimsSet -> m JwsRegisteredClaims
getRegisteredClaims claims = do
  iss <- getRegClaim claimIss "iss" claims
  aud <- getRegClaim claimAud "aud" claims
  iat <- getRegClaim claimIat "iat" claims
  jti <- getRegClaim claimJti "jti" claims
  exp <- getRegClaim claimExp "exp" claims
  return $ JwsRegisteredClaims (Just $ ClientIss iss) (Just aud) (Just iat) (Just exp) (Just $ JTI jti) where
    getRegClaim g name cs = cs ^. g & maybeErrors (_MissingClaim # name)

-- convert a signed jwt to base64 then make it a json Value (for a claim)
jwtToJson :: SignedJWT -> Value
jwtToJson = toJSON . TE.decodeUtf8 . BSL.toStrict . encodeCompact

getClaim :: forall e m a.
  ( AsError e
  , MonadError e m
  , FromJSON a
  ) =>
 AesonClaims -> T.Text -> m a
getClaim m n = m ^. at n & (>>=fromVal) . maybeErrors (_MissingClaim # n)

getmClaim :: forall e m a.
  ( AsError e
  , MonadError e m
  , FromJSON a
  ) =>
 AesonClaims -> T.Text -> m (Maybe a)
getmClaim m n = m ^. at n & traverse fromVal

fromVal :: forall e m a.
  ( AsError e
  , MonadError e m
  , FromJSON a
  ) => Value -> m a
fromVal = rToM . fromJSON where
 rToM = \case
         Error s -> throwError . (_ParseError #) $ s
         Success a -> pure a

maybeErrors :: (AsError e, MonadError e m) => e -> Maybe a -> m a
maybeErrors e = maybe (throwError e) pure

aesonClaimsToMetaData :: forall e m.
  ( AsError e
  , MonadError e m
  ) =>
 AesonClaims -> m ClientMetaData
aesonClaimsToMetaData m = do
  _clientName         <- getClaim m "client_name"
  _clientUri         <- getmClaim m "client_uri"
  _contacts         <- getmClaim m "contacts"
  _logoUri         <- getmClaim m "logo_uri"
  _policyUri         <- getmClaim m "policy_uri"
  _tosUri         <- getmClaim m "tos_uri"
  _subjectType         <- getClaim m "subject_type"
  _sectorIdentifierUri         <- getmClaim m "sector_identifier_uri"
  -- TODO: The spec on this is going to change, awaiting.
  -- mjwks         <- getmClaim m "jwks"
  -- mjwksUri       <- getmClaim m "jwks_uri"
  -- Fail if neither jwks or jwks_uri are supplied
  --_keySet <- maybeErrors (_MissingClaim "jwks or jwks_uri required.") (mjwks <|> mjwksUri)
  let _keySet = undefined
  _requestUris         <- getmClaim m "request_uris"
  _redirectUris         <- getClaim m "redirect_uris"
  ra <- getmClaim m "request_object_encryption_alg"
  re <- getmClaim m "request_object_encryption_enc"
  let _requestObjectEncryption = RequestObjectEncryption <$> ra <*> pure re
  _userinfoSignedResponseAlg         <- getmClaim m "userinfo_signed_response_alg"
  ia <- getClaim m "id_token_encrypted_response_alg"
  ie <- getClaim m "id_token_encrypted_response_enc"
  let _idTokenEncryption = IdTokenEncryption ia ie
  _responseTypes         <- getmClaim m "response_types"
  _defaultMaxAge         <- getmClaim m "default_max_age"
  _requireAuthTime         <- getmClaim m "require_auth_time"
  _defaultAcrValues         <- getmClaim m "default_acr_values"
  _initiateLoginUri         <- getmClaim m "initiate_login_uri"
  ua          <- getmClaim m "user_info_encrypted_response_alg"
  ue         <- getmClaim m "user_info_encrypted_response_enc"
  let _userInfoEncryption  = UserInfoEncryption <$> ua <*> pure ue
  _idTokenSignedResponseAlg         <- getClaim m "id_token_signed_response_alg"
  _requestObjectSigningAlg         <- getClaim m "request_object_signing_alg"
  _grantTypes         <- getmClaim m "grant_types"
  _applicationType         <- getClaim m "application_type"
  _tokenEndpointAuthMethod         <- getClaim m "token_endpoint_auth_method"
  _scope         <- getmClaim m "scope"
  _softwareId         <- getmClaim m "software_id"
  _softwareVersion         <- getmClaim m "software_version"
  _mutualTlsSenderConstrainedAccessTokens <- getClaim m "software_version"
  _clientNotificationEndpoint <- getClaim m "software_version"
  pure ClientMetaData {..}
