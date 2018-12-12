{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Web.ConsumerData.Au.Api.Types.Auth.Common.Common
  ( ErrorCode (ErrorCode)
  , ErrorDescription
  , GrantErrorResponseType (..)
  , grantErrorResponseTypeText
  , Acr (..)
  , AuthUri
  , Claim (..)
  , FapiPermittedAlg
  , getFapiPermittedAlg
  , _FapiPermittedAlg
  , HttpsUrl
  , mkHttpsUrlText
  , mkHttpsUrl
  , Hash
  , Nonce (..)
  , Prompt (..)
  , SHash
  , TokenAddressText
  , TokenAuthTime
  , TokenCHash
  , TokenErrorResponseType
  , TokenHeaders
  , TokenKeyId
  , TokenMaxAgeSeconds
  , TokenPhoneText
  , TokenSubject (..)
  , ClientId (..)
  , RedirectUri (..)
  , ResponseType (..)
  , ClientIss (..)
  -- Not exporting constructor for Scopes --- use the smart constructor
  , Scopes
  , mkScopes
  , scopeText
  , Scope (..)
  , State (..)
  ) where

import           Aeson.Helpers              (parseJSONWithPrism, parseWithPrism)
import           Control.Lens
    (Prism', prism, ( # ), (&), (<&>), (^.), (^?))
import           Control.Monad              ((<=<))
import           Control.Monad.Error.Lens   (throwing_)
import           Control.Monad.Except       (MonadError)
import           Crypto.Hash                (HashAlgorithm, hashWith)
import           Crypto.JOSE.JWA.JWS        (Alg (ES256, PS256))
import           Crypto.JWT                 (StringOrURI)
import           Data.Aeson.Types
    (FromJSON (..), FromJSON1 (..), Parser, ToJSON (..), ToJSON1 (..), Value,
    object, toJSON1, withObject, (.:), (.=))
import           Data.Bool                  (bool)
import qualified Data.ByteArray             as BA
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Base64.URL (encode)
import           Data.Char                  (isAscii)
import           Data.Functor.Classes       (Eq1 (liftEq), Show1 (..))
import           Data.Functor.Contravariant ((>$<))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           GHC.Generics               (Generic, Generic1)
import           Text.URI                   (URI, mkURI)
import qualified Text.URI                   as URI
import           Text.URI.Lens              (unRText, uriScheme)
import           Waargonaut.Encode          (Encoder')
import qualified Waargonaut.Encode          as E

import Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsHttpsUrlError (..), HttpsUrlError)

{-|

This code is based on the <https://openid.net/specs/openid-financial-api-part-2.html §Financial-grade OIDC API> spec.

-}

-- | @client_id@ field supplied in requests/responses, provided to the client after registration
newtype ClientId =
  ClientId {getClientId :: Text}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype AuthCodeVerifier =
  AuthCodeVerifier {getAuthCodeVerifier :: Text}
  deriving (Generic, ToJSON, Show, Eq)

newtype State =
  State {getState :: Text}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

newtype Nonce =
  Nonce {getNonce :: Text}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

--TODO create error types
newtype ErrorCode = ErrorCode Text


-- | The @error_description@ returned as parameter of the query component of the part of the redirection URI using the "application/x-www-form-urlencoded" format. Human-readable ASCII [USASCII] text providing additional information, used to assist the client developer in understanding the error that occurred. Values for the @error_description@ parameter MUST NOT include characters outside the set %x20-21 / %x23-5B / %x5D-7E.
newtype ErrorDescription =
  ErrorDescription {getErrorDescription :: Text}
  deriving (Generic, ToJSON)

-- | The @error_uri@ returned as parameter of the query component of the part of the redirection URI using the "application/x-www-form-urlencoded" format. A URI identifying a human-readable web page with information about the error, used to provide the client developer with additional information about the error. Values for the @error_uri@ parameter MUST conform to the URI-reference syntax and thus MUST NOT include characters outside the set %x21 / %x23-5B / %x5D-7E.
newtype AuthUri =
  AuthUri {getAuthUri :: URI}
  deriving (Eq, Show)

instance ToJSON AuthUri where
  toJSON (AuthUri uri) =
    toJSON $ URI.render uri

instance FromJSON AuthUri where
  parseJSON =
    fmap AuthUri . (>>= toParser) . fmap mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

-- | Error response types for authorization code grant and for implicit grant
-- <https://tools.ietf.org/html/rfc6749#section-4.1.2.1 OAuth 2.0 §4.1.2.1> for authorization code grant
-- <https://tools.ietf.org/html/rfc6749#section-4.2.2.1 OAuth 2.0 §4.2.2.1> for implicit grant
data GrantErrorResponseType =
  InvalidRequest
  -- ^ @invalid_request@: The request is missing a required parameter, includes an invalid parameter value, includes a parameter more than once, or is otherwise malformed.
  | UnauthorizedClient
  -- ^ @unauthorized_client@: The client is not authorized to request an authorization code (or access token) using this method.
  | AccessDenied
  -- ^ @access_denied@: The resource owner or authorization server denied the request.
  | UnsupportedResponseType
  -- ^ @unsupported_response_type@: The authorization server does not support obtaining an authorization code (or access code) using this method.
  | InvalidScope
  -- ^ @invalid_scope@: The requested scope is invalid, unknown, or malformed.
  | ServerError
  -- ^ @server_error@: The authorization server encountered an unexpected condition that prevented it from fulfilling the request. (This error code is needed because a 500 Internal Server Error HTTP status code cannot be returned to the client via an HTTP redirect.)
  | TemporarilyUnavailable
  -- ^ @temporarily_unavailable@: The authorization server is currently unable to handle the request due to a temporary overloading or maintenance of the server. (This error code is needed because a 503 Service Unavailable HTTP status code cannot be returned to the client via an HTTP redirect.)

grantErrorResponseTypeText ::
  Prism' Text GrantErrorResponseType
grantErrorResponseTypeText =
  prism (\case
            InvalidRequest -> "invalid_request"
            UnauthorizedClient -> "unauthorized_client"
            AccessDenied -> "access_denied"
            UnsupportedResponseType -> "unsupported_response_type"
            InvalidScope -> "invalid_scope"
            ServerError -> "server_error"
            TemporarilyUnavailable -> "temporarily_unavailable"
        )
        (\case
            "invalid_request" -> Right InvalidRequest
            "unauthorized_client" -> Right UnauthorizedClient
            "access_denied" -> Right AccessDenied
            "unsupported_response_type" -> Right UnsupportedResponseType
            "invalid_scope" -> Right InvalidScope
            "server_error" -> Right ServerError
            "temporarily_unavailable" -> Right TemporarilyUnavailable
            e -> Left e
        )

grantErrorResponseTypeEncoder ::
  Encoder' GrantErrorResponseType
grantErrorResponseTypeEncoder =
  (grantErrorResponseTypeText #) >$< E.text'

-- | Error response types for issuing an access token
-- <https://tools.ietf.org/html/rfc6749#section-5.2 OAuth 2.0 §5.2>
data TokenErrorResponseType =
  TokenInvalidRequest
  -- ^ @invalid_request@: The request is missing a required parameter, includes an unsupported parameter value (other than grant type), repeats a parameter, includes multiple credentials, utilizes more than one mechanism for authenticating the client, or is otherwise malformed.
  | TokenInvalidClient
  -- ^ @invalid_client@: Client authentication failed (e.g., unknown client, no client authentication included, or unsupported authentication method).  The authorization server MAY return an HTTP 401 (Unauthorized) status code to indicate which HTTP authentication schemes are supported.  If the  client attempted to authenticate via the "Authorization" request header field, the authorization server MUST respond with an HTTP 401 (Unauthorized) status code and include the "WWW-Authenticate" response header field matching the authentication scheme used by the client.
  | TokenInvalidGrant
  -- ^ @invalid_grant@: The provided authorization grant (e.g., authorization code, resource owner credentials) or refresh token is invalid, expired, revoked, does not match the redirection URI used in the authorization request, or was issued to another client.
  | TokenUnauthorizedClient
  -- ^ @unauthorized_client@: The authenticated client is not authorized to use this authorization grant type.
  | TokenUnsupportedGrantType
  -- ^ @unsupported_grant_type@: The authorization grant type is not supported by the authorization server.
  | TokenInvalidScope
  -- ^ @invalid_scope@: The requested scope is invalid, unknown, malformed, or exceeds the scope granted by the resource owner.

tokenErrorResponseTypeText ::
  Prism' Text TokenErrorResponseType
tokenErrorResponseTypeText =
  prism (\case
            TokenInvalidRequest -> "invalid_request"
            TokenInvalidClient -> "invalid_client"
            TokenInvalidGrant -> "invalid_grant"
            TokenUnauthorizedClient -> "unauthorized_client"
            TokenUnsupportedGrantType -> "unsupported_grant_type"
            TokenInvalidScope -> "invalid_scope"
        )
        (\case
            "invalid_request" -> Right TokenInvalidRequest
            "invalid_client" -> Right TokenInvalidClient
            "invalid_grant" -> Right TokenInvalidGrant
            "unauthorized_client" -> Right TokenUnauthorizedClient
            "unsupported_grant_type" -> Right TokenUnsupportedGrantType
            "invalid_scope" -> Right TokenInvalidScope
            e -> Left e
        )

tokenErrorResponseTypeEncoder ::
  Encoder' TokenErrorResponseType
tokenErrorResponseTypeEncoder =
  (tokenErrorResponseTypeText #) >$< E.text'

-- | Valid response types for FAPI R+W.
-- <https://openid.net/specs/openid-financial-api-part-2.html#authorization-server FAPI R+W §5.2.2.2>
-- mandates the use of @code id_token@ or @code id_token token@ to be supplied in @response_type@.
-- Furthermore, <https://consumerdatastandardsaustralia.github.io/standards/#additional-constraints the standards>
-- mandate that only the hybrid flow is supported, and only @code id_token@ as per the <https://consumerdatastandardsaustralia.github.io/infosec/#oidc-hybrid-flow §infosec> spec.
data ResponseType =
  CodeIdToken
  -- ^ @code id_token@ response type, implying a Hybrid flow
  --  | CodeIdTokenToken
  -- ^ @code id_token token@ response type are not supported in AU OB.
  deriving (Show, Eq)

responseTypeText ::
  Prism' Text ResponseType
responseTypeText =
  prism (\case
            CodeIdToken -> "code id_token"
            -- CodeIdTokenToken -> "code id_token token"
        )
        (\case
            "code id_token" -> Right CodeIdToken
            -- "code id_token token" -> Right CodeIdTokenToken
            t -> Left t
        )

responseTypeEncoder ::
  Encoder' ResponseType
responseTypeEncoder =
  (responseTypeText #) >$< E.text'

instance ToJSON ResponseType where
  toJSON = toJSON . (responseTypeText #)

instance FromJSON ResponseType where
  parseJSON = parseJSONWithPrism responseTypeText "ResponseType"

-- | Valid scope parameters for FAPI R+W.
-- <https://openid.net/specs/openid-connect-core-1_0.html#AuthRequestValidation §3.1.2.2. Authentication Request Validation>
-- mandates the scope to include @openid@ at a minumum (we elected to disregard the suggestion in
-- <https://openid.net/specs/openid-financial-api-part-1.html#public-client §5.2.3. Public client>
-- that it could be omitted).
--
-- <https://bitbucket.org/openid/fapi/issues/149/make-it-clear-that-the-entire-flow-is-oidc This issue>
-- suggests that FAPI R+W should only be the hybrid flow, which would mandate @openid@ as a scope.
newtype Scopes =
  Scopes (Set Scope)
  deriving (Eq, Show)

instance ToJSON Scopes where
  toJSON (Scopes s) =
    toJSON . AesonSet . Set.map (scopeText #) $ s

instance FromJSON Scopes where
  parseJSON =
    let
      missingOpenId =
        fail "'scope' claim does not include 'openid'"
      validate s =
         bool missingOpenId (pure (Scopes s)) $ Set.member OpenIdScope s
    in
      (>>= validate) . parseAesonSet scopeText "Scope"

mkScopes ::
  Set Scope
  -> Scopes
mkScopes =
  Scopes . Set.insert OpenIdScope

-- | Authorisation scopes, as defined in <https://consumerdatastandardsaustralia.github.io/standards/?swagger#authorisation-scopes §AU OB Security>
data Scope =
  OpenIdScope -- ^ Include @openid@ in @scope@ JSON value
  | ProfileScope -- ^ Include @profile@ in @scope@ JSON value, for the following claims: name, family_name, given_name
  | BasicBankAccount -- ^ Include @bank_basic_accounts@ in @scope@ JSON value
  | BankDetailedAccounts -- ^ Include @bank_detailed_accounts@ in @scope@ JSON value
  | BankTransactions -- ^ Include @bank_transactions@ in @scope@ JSON value
  | CommonBasicCustomer -- ^ Include @common_basic_customer@ in @scope@ JSON value
  | CommonDetailedCustomer -- ^ Include @common_detailed_customer@ in @scope@ JSON value

  deriving (Eq, Ord, Show)

scopeText ::
  Prism' Text Scope
scopeText =
  prism (\case
            OpenIdScope -> "openid"
            ProfileScope -> "profile"
            BasicBankAccount -> "bank_basic_accounts"
            BankDetailedAccounts  -> "bank_detailed_accounts"
            BankTransactions -> "bank_transactions"
            CommonBasicCustomer -> "common_basic_customer"
            CommonDetailedCustomer -> "common_detailed_customer"
        )
        (\case
            "openid" -> Right OpenIdScope
            "profile" -> Right ProfileScope
            "bank_basic_accounts" -> Right BasicBankAccount
            "bank_detailed_accounts" -> Right BankDetailedAccounts
            "bank_transactions" -> Right BankTransactions
            "common_basic_customer" -> Right CommonBasicCustomer
            "common_detailed_customer" -> Right CommonDetailedCustomer
            t -> Left t
        )

instance ToJSON Scope where
  toJSON = toJSON . (scopeText #)

instance FromJSON Scope where
  parseJSON = parseJSONWithPrism scopeText "Scope"

data Prompt =
  SelectAccount
  deriving (Eq, Show)

prompt ::
  Prism' Text Prompt
prompt = prism
  (\SelectAccount -> "select_account")
  (\case
      "select_account" -> Right SelectAccount
      t -> Left t
  )

instance ToJSON Prompt where
  toJSON = toJSON . (prompt #)

instance FromJSON Prompt where
  parseJSON = parseJSONWithPrism prompt "Prompt"

newtype TokenHeaders = TokenHeaders [Header]
data Header = Header {key::Text, value::Text}

-- | Use @PS256@ or @ES256@ in @alg@ for a JWT
newtype FapiPermittedAlg = FapiPermittedAlg {
  getFapiPermittedAlg :: Alg }
  deriving (Eq, Ord, Show)

_FapiPermittedAlg ::
  Prism' Alg FapiPermittedAlg
_FapiPermittedAlg =
  prism (\case
            FapiPermittedAlg a -> a
        )
        (\case
            PS256 -> Right $ FapiPermittedAlg PS256
            ES256 -> Right $ FapiPermittedAlg ES256
            t -> Left t
        )

instance ToJSON FapiPermittedAlg where
  toJSON = toJSON . (_FapiPermittedAlg #)

instance FromJSON FapiPermittedAlg where
  parseJSON = parseJSONWithPrism _FapiPermittedAlg "FapiPermittedAlg"

-- | The @aud@ for the auth request. It must include (but is not limited to) the OP's Issuer Identifier URL. See < https://openid.net/specs/openid-connect-core-1_0.html#RequestObject §6.1. Passing a Request Object by Value>
newtype AuthRequestAudience = AuthRequestAudience Text --TODO is really text?
-- | The @iss@ value for the auth request, which must be the @client_id@, unless it was signed by a different party than the RP. See <https://openid.net/specs/openid-connect-core-1_0.html#RequestObject §6.1. Passing a Request Object by Value>
-- todo: need to clarify these types, whether we need a
newtype AuthIss = AuthIss Text
newtype ClientIss = ClientIss
  { getClientIss :: StringOrURI}
  deriving (Generic, ToJSON, FromJSON, Show, Eq)

-- TODO: these are not really URIs ... URNs?
newtype RedirectUri =
  RedirectUri {getRedirectUri :: URI}
  deriving (Show, Eq)

instance ToJSON RedirectUri where
  toJSON (RedirectUri uri) =
    toJSON $ URI.render uri

instance FromJSON RedirectUri where
  parseJSON =
    fmap RedirectUri . (>>= toParser) . fmap mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

-- | A @kid@ to be returned in the token. A @kid@ is the certificate Key ID, and it must be checked to match signing cert
newtype TokenKeyId = TokenKeyId Text --TODO is really text?

-- | For URIs in open banking that must be URLs using the HTTPS scheme. Use @mkHttpsUrl@ to get one.
newtype HttpsUrl =
  HttpsUrl URI
  deriving (Eq, Show)

mkHttpsUrlText ::
  ( AsHttpsUrlError e
  , MonadError e m
  )
  => Text
  -> m HttpsUrl
mkHttpsUrlText =
  mkHttpsUrl <=< maybe (throwing_ _UriParseError) pure . mkURI

mkHttpsUrl ::
  ( AsHttpsUrlError e
  , MonadError e m
  )
  => URI
  -> m HttpsUrl
mkHttpsUrl uri =
  case uri ^. uriScheme <&> (^. unRText) of
    Just "https" -> pure $ HttpsUrl uri
    Just _       -> throwing_ _NotHttps
    Nothing      -> throwing_ _MissingScheme

instance ToJSON HttpsUrl where
  toJSON (HttpsUrl uri) =
    toJSON $ URI.render uri

instance FromJSON HttpsUrl where
  parseJSON v =  toParser =<< mkHttpsUrlText <$> parseJSON v
    where
      toParser :: Either HttpsUrlError HttpsUrl -> Parser HttpsUrl
      toParser =
        either (fail . show) pure

-- | The @iat@ value returned in a token, in seconds since epoch. @iat@ (Issued At) is used to limit the amount of time that nonces need to be stored for.
newtype TokenIssuedAt = TokenIat Int --TODO is really text?
newtype TokenIntentId = TokenIntentId Text
 -- must be Intent/Consent ID for non-identity service providers
newtype TokenSubject =
  TokenSubject Text
  deriving (Eq, Show, ToJSON, FromJSON)
-- | The @jti@, a unique indentifier for the token (for debugging/revocation), required for @client_secret_jwt@ and @private_key_jwt@ client authentication types. Note that FAPI mandates the use of either MTLS or JWTs to authenticate against the token endpoint. See <https://openid.net/specs/openid-financial-api-part-1.html#authorization-server §5.2.2. Authorization server> and <https://openid.net/specs/openid-connect-core-1_0.html#ClientAuthentication §9. Client Authentication>.
newtype TokenIdentifier = TokenIdentifier Text
-- | The @aud@ for the ID Token. It must contain (but is not limited to) the client_id <https://openid.net/specs/openid-connect-core-1_0.html#HybridIDTValidation §2. ID Token>
-- TODO: confirm if support needed for other audiences
newtype TokenAudience = TokenAudience Text
 -- when the token was created (seconds since epoch)
newtype TokenAccessTime = TokenAccessTime Int
-- how many seconds before the token expires
newtype TokenMaxAgeSeconds = TokenMaxAgeSeconds Int
newtype TokenAuthTime =
  TokenAuthTime Int
  deriving (Eq, Show, FromJSON, ToJSON)
-- when the token expires (epoch)
newtype TokenExpiry = TokenExpiry Int

-- all hashes are base64url encoding of the left-most half of the hash of the octets of the ASCII representation of the original value
newtype SHash =
  SHash Text
  deriving (Eq, Show, FromJSON, ToJSON)
-- Access Token's hash
-- data AtHash where
--   AtHash :: Hash a -> AtHash

-- instance Eq AtHash where
--   AtHash h1 == AtHash h2 = h1 == h2
    -- case (h1, h2) of
    --   (Hash a bs :: Hash a, )

-- deriving instance Show AtHash

newtype TokenCHash =
  TokenCHash Text
  deriving (Eq, Show, FromJSON, ToJSON)

newtype TokenAddressText =
  TokenAddressText Text
  deriving (Eq, Show, FromJSON, ToJSON)

newtype TokenPhoneText =
  TokenPhoneText Text
  deriving (Eq, Show, FromJSON, ToJSON)

-- TODO: confirm what LoA we'll be using, and how it is represented.
-- | The minimum ACR for AU OB is LoA3, represented by URI @urn:cds.au:cdr:3@.
newtype Acr =
  Acr Text
  deriving (Eq, Show, FromJSON, ToJSON)

-- | Hash used in OIDC, which is:
--     The base64url encoding of the left-most half of the hash of the octets of the ASCII
--     representation of the value
newtype Hash =
  Hash ByteString
  deriving (Eq, Show)

instance FromJSON Hash where
  parseJSON =
    fmap (Hash . encodeUtf8) . parseJSON

instance ToJSON Hash where
  toJSON (Hash bs) =
    toJSON $ decodeUtf8 bs

newtype Ascii =
  Ascii Text
  deriving (Eq, Show)

ascii ::
  Prism' Text Ascii
ascii =
  prism (\(Ascii t) -> t)
        (bool <$> Left <*> (Right . Ascii) <*> T.all isAscii)

mkHash ::
  HashAlgorithm a
  => a
  -> Ascii
  -> Hash
mkHash a (Ascii t) =
  let
    d = hashWith a $ encodeUtf8 t
    h = BS.take (BA.length d `div` 2) . BA.convert $ d
  in
    Hash . encode $ h

data Claim a =
  Claim
  { claimValues    :: [a]
  , claimEssential :: Bool
  }
  deriving (Eq, Show, Generic1, Functor)

instance Applicative Claim where
  pure a = Claim [a] False
  (Claim fs e1) <*> (Claim as e2) = Claim (fs <*> as) (e1 || e2)

instance ToJSON1 Claim where
  --liftToJSON = genericLiftToJSON aesonOpts
  liftToJSON _ f Claim{..} =
    object
    [ "values" .= f claimValues
    , "essential" .= claimEssential
    ]

instance Eq1 Claim where
  liftEq f (Claim xs b1) (Claim ys b2) =
    and $ (b1 == b2) : zipWith f xs ys

instance Show1 Claim where
  liftShowsPrec _ f _ (Claim as b) s =
    "Claim { claimValues = " <> f as "" <> ", claimEssntial = " <> show b <> "}" <> s

instance FromJSON1 Claim where
  liftParseJSON _ f =
    withObject "Claim" $ \o ->
      Claim
      <$> (f =<< (o .: "values"))
      <*> o .: "essential"

instance ToJSON a => ToJSON (Claim a) where
  toJSON = toJSON1

newtype AesonSet = AesonSet
  {
    fromAesonSet :: Set T.Text
  } deriving (Show)

instance ToJSON AesonSet where
  toJSON (AesonSet s) = toJSON . T.intercalate " " . Set.toList $ s

instance FromJSON AesonSet where
  parseJSON v = AesonSet . Set.fromList .  T.split (== ' ') <$> parseJSON v

parseAesonSet :: Ord a => Prism' Text a -> String -> Value -> Parser (Set a)
parseAesonSet p n = fmap Set.fromList . (>>= traverse (parseWithPrism p n)) . fmap (Set.toList . fromAesonSet) .  parseJSON

-- aesonOpts ::
--   Options
-- aesonOpts =
--   defaultOptions {fieldLabelModifier = fmap toLower . dropWhile isLower}

-- makeWrapped ''Hash
