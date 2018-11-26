{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Web.ConsumerData.Au.Api.Types.Auth.Registration where
import           Control.Applicative                       (liftA2, (<|>))
import           Control.Lens
    (Prism', at, makePrisms, prism, to, ( # ), (&), (.~), (?~), (^.), (^?),
    _Right)
import           Control.Monad.Error.Class
    (MonadError, throwError)
import           Control.Monad.Time                        (MonadTime)
import qualified Crypto.JOSE.Error                         as JE
import           Crypto.JOSE.JWA.JWE                       (Enc)
import qualified Crypto.JOSE.JWA.JWE                       as JWE
import           Crypto.JOSE.JWK                           (JWK)
import           Crypto.JOSE.JWS
    (Alg, JWSHeader, newJWSHeader)
import           Crypto.JWT
    (AsJWTError, Audience, ClaimsSet, JWT, NumericDate, SignedJWT, StringOrURI,
    claimAud, claimIss, decodeCompact, defaultJWTValidationSettings,
    emptyClaimsSet, encodeCompact, issuerPredicate, signClaims,
    unregisteredClaims, verifyClaims)
import           Crypto.Random.Types                       (MonadRandom)
import           Data.Aeson
    (FromJSON (..), Result (..), ToJSON (..), Value, fromJSON)
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
import           Text.URI                                  (URI, mkURI)
import qualified Text.URI                                  as URI
import           Text.URI.Lens
    (authHost, uriAuthority, uriScheme)
import           Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientId, FapiPermittedAlg, RedirectUri, ResponseType, Scopes,
    getRedirectUri)
import           Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsError, _MissingClaim, _ParseError)

newtype GrantTypes = GrantTypes (Set GrantType)
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Determines the credentials that will be used by a client when accessing /token endpoint. Available grant types are specified in <https://tools.ietf.org/html/rfc7591 §RFC7591 2. Client Metadata>. If not provided, server sets default of @authorization_code@.
data GrantType = Password | ClientCredentials | Implicit | AuthorizationCode | RefreshToken
  deriving (Generic, ToJSON, FromJSON, Show, Eq, Ord)

-- TODO: is `client_credentials` required/supported in FAPI, as per grant_types in OIDC-R? The UK OB model requires it in order for the client to submit a JWT to the /token endpoint to obtain a consent ID / payment ID before sending the request to /payments. Also, it makes sense that we have refresh_token as well?
-- | This must only be @authorization_code@ for token requests (<https://tools.ietf.org/html/rfc6749 RFC6749 4.1.3>), as FAPI does include specific mention of @grant_types@.
newtype FapiGrantTypes = FapiGrantTypes GrantTypes
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
-- | Smart constructor for producing FAPI permitted grant types
fapiGrantTypes :: GrantTypes -> Maybe FapiGrantTypes
fapiGrantTypes (GrantTypes grantTypes) =  if grantTypes `isSubsetOf` permittedGrantTypes then Just . FapiGrantTypes . GrantTypes $ grantTypes else Nothing where
  permittedGrantTypes = Set.fromList [AuthorizationCode]

data ApplicationType = Web | Native
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
newtype FapiApplicationType = FapiApplicationType ApplicationType
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Smart constructor for producing FAPI permitted @application_type@s. In the current version of AU OB, only @web@ is allowed.
fapiApplicationType :: ApplicationType -> Maybe FapiApplicationType
fapiApplicationType a = case a of
                           Web -> Just . FapiApplicationType $ Web
                           _   -> Nothing

-- TODO: Use an email type.
newtype RegistrationContacts = RegistrationContacts [EmailAddress]
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype EmailAddress = EmailAddress T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: JSON encoder/decoder must support BCP47 [RFC5646] language tags in keys.
-- e.g. client_name#ja-Jpan-JP :: クライアント名", as per https://openid.net/specs/openid-connect-registration-1_0.html#LanguagesAndScripts
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

--TODO: confirm what subject_type's are supported in FAPI
-- @subject_type@ requested for responses to the client
data SubjectType = Pairwise | Public
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
  , idTokenEnc :: Maybe FapiEnc
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

data TokenEndpointAuthMethod = ClientSecretPost | ClientSecretBasic | ClientSecretJwt FapiPermittedAlg | PrivateKeyJwt FapiPermittedAlg | None
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Only ClientSecretJwt and PrivateKeyJwt are supported by FAPI. @token_endpoint_auth_signing_alg@ may be required to be specified depending on the specified authentication method. All token requests will be rejected by the server if they are not signed by the algorithm specified in @alg@, or if they are signed with @none@.
newtype FapiTokenEndpointAuthMethod = FapiTokenEndpointAuthMethod TokenEndpointAuthMethod
  deriving (Generic, ToJSON, FromJSON, Show, Eq)
fapiTokenEndpointAuthMethod :: TokenEndpointAuthMethod -> Maybe FapiTokenEndpointAuthMethod
fapiTokenEndpointAuthMethod = (^?_FapiTokenEndpointAuthMethod)

_FapiTokenEndpointAuthMethod :: Prism' TokenEndpointAuthMethod FapiTokenEndpointAuthMethod
_FapiTokenEndpointAuthMethod = prism (\case
          FapiTokenEndpointAuthMethod f -> f
      )
      (\case
          ClientSecretJwt f -> Right . FapiTokenEndpointAuthMethod . ClientSecretJwt $ f
          PrivateKeyJwt f -> Right . FapiTokenEndpointAuthMethod . PrivateKeyJwt $ f
          e -> Left e
      )

-- TODO: confirm whether all `enc` values are indeed supported here. Nothing in FAPI references these, nor UK OB.
-- | FAPI accepted algorithms for content encryption, based on <https://tools.ietf.org/html/rfc7518 §RFC 7518 5. Cryptographic Algorithms for Content Encryption>.
data FapiEnc =
      A128CBC_HS256
    | A192CBC_HS384
    | A256CBC_HS512
    | A128GCM
    | A192GCM
    | A256GCM
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

fapiEnc :: Prism' Enc FapiEnc
fapiEnc =
   prism (\case
            A128CBC_HS256 -> JWE.A128CBC_HS256 -- Unsure if the CBC ciphers are acceptable
            A192CBC_HS384 -> JWE.A192CBC_HS384
            A256CBC_HS512 -> JWE.A256CBC_HS512
            A128GCM -> JWE.A128GCM
            A192GCM -> JWE.A192GCM
            A256GCM -> JWE.A256GCM
         )
         (\case
            JWE.A128CBC_HS256 -> Right A128CBC_HS256
            JWE.A192CBC_HS384->  Right A192CBC_HS384
            JWE.A256CBC_HS512-> Right  A256CBC_HS512
            JWE.A128GCM -> Right A128GCM
            JWE.A192GCM -> Right A192GCM
            JWE.A256GCM -> Right A256GCM
            -- e -> Left e
         )

-- | @default_max_age@, in seconds.
newtype DefaultMaxAge  = DefaultMaxAge Int
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | @jti@ JWT param
newtype JTI = JTI T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

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

-- | 'RedirectUrls' is a non-empty array of redirection URI values used by the client to match against supplied @redirect_uri@ request parameter. If using @web@ scheme (as per AU OB) these must all be HTTPS, and must not use 'localhost' as hostname.
newtype RedirectUrls = RedirectUrls {
  getRedirectUrls  :: [RedirectUri]}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | Constructor for @redirect_url@ array; all URLs must be HTTPS, none may be localhost, as mandated by AU OB.
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

--TODO: use Auth.Common's values here
newtype FapiAcrValues = FapiAcrValues T.Text
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | The client registration endpoint is an OAuth 2.0 endpoint that is designed to allow a client to be dynamically registered with the authorization server. 'RegistrationRequest ' represents an <https://openid.net/specs/openid-connect-registration-1_0.html §OIDC registration> such a request, with additional elements included from the <https://tools.ietf.org/html/rfc7591 §RFC7591 - OAuth 2.0 Dynamic Client Registration Protocol>. A RP will submit this request to a OP to receive client_id credentials.The registration request parameters are sent in JSON, and include a signed software statement assertion. UK OB mandates that all registration requests are also signed by the RP. NB: OIDC FAPI nor AU OB does not (yet) specify the details of dynamic client registration.
data RegistrationRequest = RegistrationRequest {
    _regoReqSigningData      :: JwsSigningHeaders
  , _regReqClientMetaData    :: ClientMetaData
  , _regReqsoftwareStatement :: RegoReqSoftwareStatement -- ^ A signed JWT containing metadata about the client software. RFC7591 mandates that this is a JWS. UK OB mandates this is supplied, and provides them through the OB directory.
  --TODO: Confirm AUD OB does the same, or allows generation.
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | These fields are required iff the AU OB directory generates software statements for the RP to submit during registration (as per the UK OB model). -- TODO: Confirm.
data JwsSigningHeaders = JwsSigningHeaders
  {
    _iss :: Maybe StringOrURI -- ^ The name of the RP. RFC7591 2.3 mandates issuer claim is present if supplied in a software statement.
  , _aud :: Maybe Audience -- ^ The OP.
  , _iat :: Maybe NumericDate -- ^ Issued at.
  , _exp :: Maybe NumericDate -- ^ Expiration time.
  , _jti :: Maybe JTI -- ^ JWT ID.
} deriving (Generic, ToJSON, FromJSON, Show, Eq)

--TODO: Smart constructor for SSA iff RP creates own SSAs
-- ssSigningData iss =

-- | The following claims are specified in <https://tools.ietf.org/html/rfc7591 §RFC7591: 3.1 Client Registration Request> to be used in both the registration request object and the software statement, all of them as optional. However, some fields are non-optional under FAPI. OZ OB client implementation will assume all supplied fields are to be included in the both the request and the software statement.
data ClientMetaData = ClientMetaData {
    _requestObjectSigningAlg        :: FapiPermittedAlg  -- The `alg` algorithm that must be used for signing request objects sent to the OP.
  , _grantTypes                     :: FapiGrantTypes -- ^ The set of grant types that a client will restrict itself to using (see <https://openid.net/specs/openid-connect-registration-1_0.html §OIDC-R 2. Client Metadata> and <https://tools.ietf.org/html/rfc7591 §RFC7591 2. Client Metadata>). FAPI restricts these.
  , _applicationType                :: FapiApplicationType -- ^ Kind of the application. AU OB mandates this to be just @web@ (i.e no @native@ apps are allowed).
  , _tokenEndpointAuthMethod        :: FapiTokenEndpointAuthMethod -- ^ Specifies which token endpoint authentication method the client will use, and also the algorithm (@token_endpoint_auth_signing_alg@) that must be used for signing if a JWT auth method is used. UK OB mandates this field is supplied.
  , _clientName                     :: Maybe Script -- ^ Human-readable name of the client to be presented to the end user.
  , _clientUri                      :: Maybe ScriptUri -- ^ URL of the home page of the client.
  , _contacts                       :: Maybe RegistrationContacts  -- ^ Array of e-mail addresses of people responsible for the client.
  , _logoUri                        :: Maybe ScriptUri -- ^ URL that references a logo for the client application.
  , _policyUri                      :: Maybe ScriptUri -- ^ URL that the client provides to the end user in order to read about the how the user data will be used.
  , _tosUri                         :: Maybe ScriptUri -- ^ URL that the client provides to the end user to read about the client's terms of service.
  , _subjectType                    :: Maybe SubjectType -- ^ Requested for responses to this client.
  , _sectorIdentifierUri            :: Maybe HttpsUrl -- ^ References an HTTPS URL of a remote file containing a single JSON array of @redirect_uri@ values.
  , _regoReqKeySet                  :: Maybe JwkSet -- ^ Either the @jwks_uri@ or @jwks@ parameter specifying client's JWKS.
  , _requestUris                    :: Maybe RequestUris -- ^ Array of @request_uri@ values that are pre-registered by the RP for use at the OP, which may cache their contents and not retrieve them at the time they are used in a request.
  , _redirectUris                   :: RedirectUrls -- ^ Non-empty array of redirection URI values used by the client to match against supplied @redirect_uri@ request parameter. If using @web@ scheme these must all be HTTPS, and must not use 'localhost' as hostname. Mandatory field, according to OIDC-R. FAPI restricts these to all be HTTPS.
  , _regoReqRequestObjectEncryption :: Maybe RequestObjectEncryption -- ^ The @request_object_encryption_alg@ and @request_object_encryption_enc@ parameters for specifying the JWS `alg` and `enc` algorithms (that might be) used when encrypting request objects. If both signed and encrypted, signing will occur first, and then encryption, with the result being a nested JWT. Warning: a RP can supply unencrypted requests, even if this is present, as this is only a declaration that the RP 'might' encrypt request objects. See 'RequestObjectEncryption' for more info.
  , _userinfoSignedResponseAlg      :: Maybe FapiPermittedAlg -- ^ The @userinfo_signed_response_alg@ parameter for specifying which JWS `alg` algorithm should be used for signing UserInfo responses.
  , _regoReqIdTokenEncrypt          :: Maybe IdTokenEncryption -- ^ The @id_token_encrypted_response_alg@ and @id_token_encrypted_response_enc@ values for specifying how the ID token should be encrypted; see 'IdTokenEncryption' for more information.
  , _responseTypes                  :: Maybe FapiResponseTypes -- ^ JSON array containing a list of OAuth 2.0 @response_type@ values that the client will restrict itself to using; defaults to @code id_token@ if omitted. FAPI restricts these. --TODO: confirm default - should be @code id_token@?
  , _defaultMaxAge                  :: Maybe DefaultMaxAge -- ^ Specifies that the end user must be actively authenticated if the end user was authenticated longer than the specified number of seconds ago; the @max_age@ request object parameter overides this.
  , _requireAuthTime                :: Maybe Bool -- ^ Whether the @auth_time@ claim in the ID token is required, defaults to False.
  , _defaultAcrValues               :: Maybe FapiAcrValues -- ^ Default 'Authentication Context Class Reference' (ACR) values for requests, values ordered by preference; defaults overridden by supplied values. --TODO: confirm whether this has any effect at the server, as ACRs are mandatory in FAPI request objects.
  , _initiateLoginUri               :: Maybe HttpsUrl -- ^ HTTS URL which can be used by a third-party to initiate a login by the RP.
  , _regoReqUserInfoEncryption      :: Maybe UserInfoEncryption -- ^ The @userinfo_encrypted_response_alg@ and @userinfo_encrypted_response_enc@ values for specifying how UserInfo response should be encrypted; see 'UserInfoEncryption' for more information.
  , _idTokenSignedResponseAlg       :: FapiPermittedAlg  -- ^ The `alg` for signing the ID token issued to this client. NB: OIDC-R allows this to be optional (see <https://openid.net/specs/openid-connect-registration-1_0.html §2. Client Metadata>), but using FAPI implies it is mandatory. --TODO confirm with AU OB spec.
  , _scope                          :: Maybe FapiScopes -- ^ A set of scopes, containing at least @openid@`.
  , _softwareId                     :: Maybe SoftwareId -- ^ Unique identifier string for the client, which should remain the same across all instances of the client software, and all versions of the client software. This must match the software ID in the SSA if supplied.
  , _softwareVersion                :: Maybe SoftwareVersion  -- ^  The version number of the software should a TPP choose to register and / or maintain it.

} deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: Collapse this depending on the AU OB spec.
data RegoReqSoftwareStatement = SuppliedSs T.Text | GenerateSs SoftwareStatement
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

data SoftwareStatement = SoftwareStatement {
    _ssSigningData :: JwsSigningHeaders
  , _ssClaims      :: ClientMetaData -- ^ All the claims to include in the SSA; RFC7591 allows any/all that are included in the request object, however SSA claims take precedence over plain JSON elements (3.1.1).
}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- | The folowing Software Certificate Assertion fields are specified by UK OB. See <https://openbanking.atlassian.net/wiki/spaces/DZ/pages/36667724/The+OpenBanking+OpenID+Dynamic+Client+Registration+Specification+-+v1.0.0-rc2 §UK OB Spec> for more information) (NB: the OB naming convention deviates from OIDC-R/RFC7591 in that it includes a `software_` prefix in the field keys.)
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

-- | Response type for a dynamic registration request.
data RegistrationResponse = RegistrationResponse
  {
    _clientId                 :: ClientId -- ^ OAuth 2.0 client identifier string. RFC7591 mandated.
  , _clientSecret             :: Maybe ClientSecret -- ^ OAuth 2.0 client secret string, used by confidential clients to authenticate to token end-point.
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

--TODO: impl
-- registrationErrorType ::
--   Prism' T.Text RegistrationErrorType
-- registrationErrorType =
--   prism (\case
--             InvalidRequest -> "invalid_request"
--             UnauthorizedClient -> "unauthorized_client"
--             AccessDenied -> "access_denied"
--             UnsupportedResponseType -> "unsupported_response_type"
--             InvalidScope -> "invalid_scope"
--             ServerError -> "server_error"
--             TemporarilyUnavailable -> "temporarily_unavailable"
--         )
--         (\case
--             "invalid_request" -> Right InvalidRequest
--             "unauthorized_client" -> Right UnauthorizedClient
--             "access_denied" -> Right AccessDenied
--             "unsupported_response_type" -> Right UnsupportedResponseType
--             "invalid_scope" -> Right InvalidScope
--             "server_error" -> Right ServerError
--             "temporarily_unavailable" -> Right TemporarilyUnavailable
--             e -> Left e
--         )

-- registrationErrorTypeEncoder ::
--   Encoder' RegistrationErrorType
-- registrationErrorTypeEncoder  =
--   (RegistrationError#) >$< E.text'

type AesonClaims = HashMap T.Text Value

-- Jose needs JWT claims as Aeson Values
metaDataToAesonClaims ::
  ClientMetaData
  -> AesonClaims
metaDataToAesonClaims ClientMetaData{..} =
  M.empty
    & at "client_name".~ (toJSON <$> _clientName)
    & at "client_uri".~ (toJSON <$> _clientUri)
    & at "contacts".~ (toJSON <$> _contacts)
    & at "logo_uri".~ (toJSON <$> _logoUri)
    & at "policy_uri".~ (toJSON <$> _policyUri)
    & at "tos_uri".~ (toJSON <$> _tosUri)
    & at "subject_type".~ (toJSON <$> _subjectType)
    & at "sector_identifier_uri".~ (toJSON <$> _sectorIdentifierUri)
    & at "jwks".~ (toJSON <$> _regoReqKeySet)
    & at "jwks_uri".~ (toJSON <$> _regoReqKeySet)
    & at "request_uris".~ (toJSON <$> _requestUris)
    & at "redirect_uris" ?~ toJSON _redirectUris
    & at "request_object_encryption_alg".~ (toJSON  . reqObjAlg <$> _regoReqRequestObjectEncryption)
    & at "request_object_encryption_enc".~ (toJSON . reqObjEnc <$> _regoReqRequestObjectEncryption)
    & at "userinfo_signed_response_alg".~ (toJSON <$> _userinfoSignedResponseAlg)
    & at "id_token_encrypted_response_alg".~ (toJSON . idTokenAlg <$> _regoReqIdTokenEncrypt)
    & at "id_token_encrypted_response_enc".~ (toJSON . idTokenEnc <$> _regoReqIdTokenEncrypt)
    & at "response_types".~ (toJSON <$> _responseTypes)
    & at "default_max_age".~ (toJSON <$> _defaultMaxAge)
    & at "require_auth_time".~ (toJSON <$> _requireAuthTime)
    & at "default_acr_values".~ (toJSON <$> _defaultAcrValues)
    & at "initiate_login_uri".~ (toJSON <$> _initiateLoginUri)
    & at "user_info_encrypted_response_alg".~ (toJSON . userInfoAlg <$> _regoReqUserInfoEncryption)
    & at "user_info_encrypted_response_enc".~ (toJSON . userInfoEnc <$> _regoReqUserInfoEncryption)
    & at "id_token_signed_response_alg"?~ toJSON _idTokenSignedResponseAlg
    & at "request_object_signing_alg"?~ toJSON _requestObjectSigningAlg
    & at "grant_types"?~ toJSON _grantTypes
    & at "application_type"?~ toJSON _applicationType
    & at "token_endpoint_auth_method"?~ toJSON _tokenEndpointAuthMethod
    & at "scope".~ (toJSON <$> _scope)
    & at "software_id".~ (toJSON <$> _softwareId)
    & at "software_version".~ (toJSON <$> _softwareVersion)

-- | Currently all meta-data is included in the software statement. TODO: Verify this should be the case, after spec is formalised.
ssToAesonClaims ::
  SoftwareStatement
  -> AesonClaims
ssToAesonClaims = metaDataToAesonClaims . _ssClaims

-- TODO: NB: It is not yet specified whether SSAs will be generated by the directory. If they are, the registration request may need to be signed by the RP, and the `software_statement` will just be an pre-encoded JWS text value, rather than a generated JWT.
-- | Sign a registration request for sending to OP, and produce a signed software statement.
regoReqToJwt ::
  ( MonadRandom m
  , MonadError e m
  , AsError e
  , JE.AsError e
  )
  => JWK
  -> Alg
  -> RegistrationRequest
  -> m SignedJWT
regoReqToJwt jwk alg rr =
  let
    mkCs h m = emptyClaimsSet & addClaimHeaders h & unregisteredClaims .~ m
    ssClaims ssreg = mkCs (_ssSigningData ssreg) (ssToAesonClaims ssreg)
    reqAcm = metaDataToAesonClaims . _regReqClientMetaData $ rr
    --TODO: If necessary, remove ssb64 and just use the SS in the meta data, if SS creation not reqd.
    reqClaims ssb64 = mkCs (_regoReqSigningData rr) (reqAcm & at "software_statement" ?~ ssb64)
  in do
    -- sign the software statement and get the b64 encoded JWS as an aeson Value
    ssb64 <- case _regReqsoftwareStatement rr of
                  SuppliedSs ss -> return $ toJSON ss
                  GenerateSs ss -> jwtToJson <$> signClaims jwk (newJWSHeader ((), alg)) (ssClaims ss)
    -- .. and now sign the rego reuest
    signClaims jwk (newJWSHeader ((), alg)) (reqClaims ssb64)

addClaimHeaders :: JwsSigningHeaders -> ClaimsSet -> ClaimsSet
addClaimHeaders h claims = claims
      & claimIss .~ _iss h
      & claimAud .~ _aud h
      -- TODO: Add support for iat, jti, exp?

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
    -- TODO: we need seperate predicates here for the JWT?
    validationSettings = defaultJWTValidationSettings audPred & issuerPredicate .~ issPred
    c2m c = c ^. unregisteredClaims . to aesonClaimsToMetaData
  claims <- verifyClaims validationSettings jwk jwt
  ssjwt <- decodeCompact =<< BSL.fromStrict . TE.encodeUtf8 <$> claims ^. unregisteredClaims . to (`getClaim` "software_statement")
  ssclaims <- verifyClaims validationSettings jwk ssjwt
  -- Get the `software_statement` (ie a JWT), extract the headers and the claims
  ss <-  SoftwareStatement <$> getSigningHeaders ssclaims <*> c2m ssclaims
  -- ... putting that inside the software statement in the RegistrationRequest
  RegistrationRequest <$> getSigningHeaders claims <*> c2m claims <*> pure (GenerateSs ss)

getSigningHeaders :: (  MonadError e m
                    , AsError e
                    , AsJWTError e
                    , JE.AsError e
                    , MonadTime m
                    ) => ClaimsSet -> m JwsSigningHeaders
getSigningHeaders claims = do
  iss <- getRegClaim claimIss "issuer" claims
  aud <- getRegClaim claimAud "aud" claims
  --TODO: get the other claims here as well
  return $ JwsSigningHeaders (Just iss) (Just aud) Nothing Nothing Nothing
    where
    getRegClaim g name cs = cs ^. g & maybeErrors (_MissingClaim # name)

-- convert a signed jwt to JSON, encode it, then make it a json Value (for a claim)
jwtToJson :: SignedJWT -> Value
jwtToJson = toJSON . TE.decodeUtf8 . BSL.toStrict . encodeCompact

instance ToJSON (JWT (a JWSHeader)) where
  toJSON = toJSON

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

maybeErrors :: MonadError e m => e -> Maybe a -> m a
maybeErrors e = maybe (throwError e) pure

aesonClaimsToMetaData :: forall e m.
  ( AsError e
  , MonadError e m
  ) =>
 AesonClaims -> m ClientMetaData
aesonClaimsToMetaData m = do
  _clientName         <- getmClaim m "client_name"
  _clientUri         <- getmClaim m "client_uri"
  _contacts         <- getmClaim m "contacts"
  _logoUri         <- getmClaim m "logo_uri"
  _policyUri         <- getmClaim m "policy_uri"
  _tosUri         <- getmClaim m "tos_uri"
  _subjectType         <- getmClaim m "subject_type"
  _sectorIdentifierUri         <- getmClaim m "sector_identifier_uri"
  mjwks         <- getmClaim m "jwks"
  mjwksUri       <- getmClaim m "jwks_uri"
  -- TODO: fail if both supplied
  let _regoReqKeySet = mjwks <|> mjwksUri
  _requestUris         <- getmClaim m "request_uris"
  _redirectUris         <- fromVal "redirect_uris"
  ra <- fromVal "request_object_encryption_alg"
  re <- getmClaim m "request_object_encryption_enc"
  let _regoReqRequestObjectEncryption = Just $ RequestObjectEncryption ra re
  _userinfoSignedResponseAlg         <- getmClaim m "userinfo_signed_response_alg"
  ia <- fromVal "id_token_encrypted_response_alg"
  ie <- getmClaim m "id_token_encrypted_response_enc"
  let _regoReqIdTokenEncrypt = Just $ IdTokenEncryption ia ie
  _responseTypes         <- getmClaim m "response_types"
  _defaultMaxAge         <- getmClaim m "default_max_age"
  _requireAuthTime         <- getmClaim m "require_auth_time"
  _defaultAcrValues         <- getmClaim m "default_acr_values"
  _initiateLoginUri         <- getmClaim m "initiate_login_uri"
  ua          <- fromVal "user_info_encrypted_response_alg"
  ue         <- getmClaim m "user_info_encrypted_response_enc"
  let _regoReqUserInfoEncryption  = Just $ UserInfoEncryption ua ue
  _idTokenSignedResponseAlg         <- fromVal "id_token_signed_response_alg"
  _requestObjectSigningAlg         <- fromVal "request_object_signing_alg"
  _grantTypes         <- fromVal "grant_types"
  _applicationType         <- fromVal "application_type"
  _tokenEndpointAuthMethod         <- fromVal "token_endpoint_auth_method"
  _scope         <- getmClaim m "scope"
  _softwareId         <- getmClaim m "software_id"
  _softwareVersion         <- getmClaim m "software_version"
  pure ClientMetaData {..}
