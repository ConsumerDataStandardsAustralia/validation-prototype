{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Web.ConsumerData.Au.Api.Types.Auth.AuthorisationRequest
  (
  -- * AuthorisationRequest
  AuthorisationRequest (..)

  -- * AuthorisationRequest encoding/decoding
  , authRequestToJwt
  , jwtToAuthRequest

  -- * Claims
  , Claims (..)
  )
  where

import Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientId (getClientId), Nonce, RedirectUri, ResponseType, Scopes, State, Prompt)
import Web.ConsumerData.Au.Api.Types.Auth.Common.IdToken (IdTokenClaims)
import Web.ConsumerData.Au.Api.Types.Auth.Error
    (AsError, _MissingClaim, _ParseError)

import           Control.Lens
    (at, makeClassy, to, ( # ), (&), (.~), (?~), (^.), (^?))
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.Time        (MonadTime)
import qualified Crypto.JOSE.Error         as JE
import           Crypto.JOSE.JWK           (JWK)
import           Crypto.JOSE.JWS           (Alg, newJWSHeader)
import           Crypto.JWT
    (AsJWTError, Audience, SignedJWT, StringOrURI, claimAud, claimIss,
    defaultJWTValidationSettings, emptyClaimsSet, issuerPredicate, signClaims,
    unregisteredClaims, verifyClaims, stringOrUri)
import           Crypto.Random.Types       (MonadRandom)
import           Data.Aeson
    (FromJSON (..), Result (..), ToJSON (..), Value, fromJSON, object,
    withObject, (.:), (.=))
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as M
import           Data.Text                 (Text)

-- | @id_token@ claim required, as
-- <https://openid.net/specs/openid-financial-api-part-2.html#public-client FAPI R+W §5.2.3.3>
-- requires the @acr@ claim is requested
data Claims =
  Claims
  { _claimsUserInfo :: Maybe UserInfoClaim
  , _claimsIdToken  :: IdTokenClaims
  }
  deriving (Eq, Show)

-- | JSON key names come from <https://openid.net/specs/openid-connect-core-1_0.html#ClaimsParameter OIDC §5.5>
instance FromJSON Claims where
  parseJSON = withObject "Claims" $ \o ->
    Claims
    <$> o .: "userinfo"
    <*> o .: "id_token"

instance ToJSON Claims where
  toJSON Claims{..} = object
    [ "userinfo" .= _claimsUserInfo
    , "id_token" .= _claimsIdToken
    ]

-- Intent ID must be included in the UserInfo claim
newtype UserInfoClaim =
  UserInfoClaim Text
  deriving (Eq, Show, FromJSON, ToJSON)

-- | Represents an OIDC authorisation request. In response to a request from an end user, an RP
-- will redirect an end user to the OP with a request that includes this information.
--
--   * <https://consumerdatastandardsaustralia.github.io/infosec/#oidc-hybrid-flow INFOSEC>
--     is the authoritative source on these parameters.
--   * Request params using the OAuth 2.0 syntax are superseded by those in the request object
--     (@request@ or @request_uri@ fields)
--     <https://openid.net/specs/openid-connect-core-1_0.html#RequestObject 6.1. Passing a Request Object by Value>
--   * Supplying parameters (state, nonce, etc) as HTTP params allows them vary with each request
--     (and facilitates caching)
--   * @response_type@ and @client_id@ MUST match those in the request object
--     <https://openid.net/specs/openid-connect-core-1_0.html#RequestObject OIDC §6.1>
--   * <https://openid.net/specs/openid-financial-api-part-2.html#public-client FAPI R+W §5.2.3.8>
--     mandates that all request parameters are sent inside a request object.
--   * <https://openid.net/specs/openid-financial-api-part-2.html#public-client FAPI R+W §5.2.3.9>
--     mandates that duplicates of any values required by OAuth 2.0 are included in the request.
data AuthorisationRequest =
  AuthorisationRequest
  { _authReqResponseType :: ResponseType
  , _authReqClientId     :: ClientId
  , _authReqRedirectUri  :: RedirectUri
  , _authReqScope        :: Scopes
  , _authReqState        :: State
  -- | @nonce@ is required given <https://openid.net/specs/openid-financial-api-part-1.html#public-client FAPI RO §5.2.3.8>.
  -- We currently assume that a persistent identifier is required.
  , _authReqNonce        :: Nonce
  , _authReqPrompt       :: Prompt
  , _authReqAudience     :: Audience
  -- | @claims@ required, as <https://openid.net/specs/openid-financial-api-part-2.html#public-client FAPI R+W §5.2.3.3>
  -- requires the @acr@ claim is requested, which is a part of the @id_token@ claim
  , _authReqClaims       :: Claims
  }
  deriving (Show, Eq)

makeClassy ''AuthorisationRequest

authRequestToAesonMap ::
  AuthorisationRequest
  -> HashMap Text Value
authRequestToAesonMap ar =
  M.empty
  & at "response_type" ?~ toJSON (ar ^. authReqResponseType)
  & at "client_id" ?~ toJSON (ar ^. authReqClientId)
  & at "redirect_uri" ?~ toJSON (ar ^. authReqRedirectUri)
  & at "scope" ?~ toJSON (ar ^. authReqScope)
  & at "state" ?~ toJSON (ar ^. authReqState)
  & at "nonce" ?~ toJSON (ar ^. authReqNonce)
  & at "prompt" ?~ toJSON (ar ^. authReqPrompt)
  & at "claims" ?~ toJSON (ar ^. authReqClaims)

aesonMapToAuthRequest ::
  forall e m.
  ( AsError e
  , MonadError e m
  )
  => HashMap Text Value
  -> Audience
  -> m AuthorisationRequest
aesonMapToAuthRequest m aud = do
  let
    get name =
      let
        m2m = maybe (throwError $ _MissingClaim # name) pure
        mma = m ^. at name & traverse (rToM . fromJSON)
      in
        m2m =<< mma
    rToM = \case
      Error s -> throwError $ _ParseError # s
      Success a -> pure a
  AuthorisationRequest
    <$> get "response_type"
    <*> get "client_id"
    <*> get "redirect_uri"
    <*> get "scope"
    <*> get "state"
    <*> get "nonce"
    <*> get "prompt"
    <*> pure aud
    <*> get "claims"

authRequestToJwt ::
  ( MonadRandom m
  , MonadError e m
  , AsError e
  , JE.AsError e
  )
  => JWK
  -> Alg
  -> AuthorisationRequest
  -> m SignedJWT
authRequestToJwt jwk alg ar@AuthorisationRequest{..} = do
  let
    arMap = authRequestToAesonMap ar
    mIss = getClientId _authReqClientId ^? stringOrUri
    badIss = "'" <> getClientId _authReqClientId <> "' is not a valid client_id/iss"
    cs = emptyClaimsSet
      & claimAud ?~ _authReqAudience
      & unregisteredClaims .~ arMap
  iss <- maybe (throwError $ _ParseError # show badIss) pure mIss
  let cs' = cs & claimIss ?~ iss
  signClaims jwk (newJWSHeader ((), alg)) cs'

jwtToAuthRequest ::
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
  -> m AuthorisationRequest
jwtToAuthRequest audPred issPred jwk jwt = do
  let
    validationSettings =
      defaultJWTValidationSettings audPred
      & issuerPredicate .~ issPred
    getRegClaim g name cs =
      cs ^. g & maybe (throwError $ _MissingClaim # name) pure
  claims <- verifyClaims validationSettings jwk jwt
  aud <- getRegClaim claimAud "aud" claims
  claims ^. unregisteredClaims . to (`aesonMapToAuthRequest` aud)
