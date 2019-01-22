{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Web.ConsumerData.Au.Api.Types.Auth.Common.IdToken where

import           Crypto.JWT          (Audience)
import           Data.Aeson
    (FromJSON (..), ToJSON (..), Value (Object), parseJSON1, withObject, (.:),
    (.:?))
import           Data.Aeson.Types    (FromJSON1, Parser, ToJSON1, Value (Null))
import           Data.Constraint     (Dict (Dict))
import           Data.Dependent.Map  (DMap)
import qualified Data.HashMap.Strict as HM
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH    (deriveGShow)
import Data.Functor.Identity (Identity)
import Data.Some             (Some (This))
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Text.URI              (URI)

import Data.GADT.Aeson    (JSONKey (..), mkParseJSON, toJSONDMap, toObjectDMap)
import Data.GADT.Aeson.TH (deriveFromJSONViaKey, deriveToJSONViaKey)
import Data.GADT.Tag.TH   (deriveEqTag, deriveShowTag)

import Web.ConsumerData.Au.Api.Types.Auth.Common.Claims (Claim)
import Web.ConsumerData.Au.Api.Types.Auth.Common.Common
    (Acr, AuthUri, FapiPermittedAlg, Hash, Nonce, SHash,
    TokenAddressText, TokenAuthTime, TokenCHash, TokenHeaders, TokenKeyId,
    TokenPhoneText, TokenSubject, ConsentId)

-- AuthZ server can respond with either JWS or JWE ID tokens
data IdTokenJwt =
  IdTokenJws IdTokenHeaders IdToken'
  | IdTokenJwe IdTokenHeaders IdToken'

-- headers for the ID token response
data IdTokenHeaders = IdTokenHeaders {
   _alg               :: FapiPermittedAlg
  ,_kid               :: TokenKeyId
  , additionalHeaders :: TokenHeaders -- can specify: `typ` but must be JWT (?)
}

-- TODO ajmcmiddlin: why are there Maybe values here? It's a map, so they're all optional to begin with.
-- | ID Token for Hybrid flow. Most of this structure comes from
-- <https://openid.net/specs/openid-connect-core-1_0.html#IDToken OIDC §2>.
--
-- <https://openid.net/specs/openid-connect-core-1_0.html#HybridIDToken OIDC §3.3.2.11> states that:
--   * @nonce@ is mandatory in ID tokens.
--   * @at_hash@ is only required when an access token is returned from the authorisation endpoint.
--   * @c_hash@ is required for any hybrid flow.
data IdTokenKey a where
  IdTokenIss      :: IdTokenKey AuthUri
  IdTokenSub      :: IdTokenKey TokenSubject
  IdTokenAud      :: IdTokenKey Audience
  IdTokenExp      :: IdTokenKey UTCTime
  IdTokenIat      :: IdTokenKey UTCTime
  IdTokenAuthTime :: IdTokenKey (Maybe TokenAuthTime) -- required when max_age request made or included in essential claims
  IdTokenSHash    :: IdTokenKey SHash -- recommended
  IdTokenAtHash   :: IdTokenKey (Maybe Hash)
  IdTokenAddress  :: IdTokenKey (Maybe TokenAddressText)
  IdTokenPhone    :: IdTokenKey (Maybe TokenPhoneText)

deriving instance Eq (IdTokenKey a)
deriving instance Show (IdTokenKey a)

data IdToken (g :: IdTokenUse) =
  IdToken
  { idTokenNonce :: IdTokenUseF g Nonce
  , idTokenCHash :: IdTokenUseF g TokenCHash
  , idTokenAcr   :: IdTokenUseF g [Acr]
  , idTokenConsentId :: IdTokenUseF g ConsentId
  , idTokenMap   :: DMap IdTokenKey (IdTokenMapFunctor g)
  }

deriving instance Eq IdToken'
deriving instance Show IdToken'
deriving instance Eq IdTokenClaims
deriving instance Show IdTokenClaims

data IdTokenUse =
  ClaimUse
  | TokenUse

type family IdTokenUseF (use :: IdTokenUse) a where
  IdTokenUseF 'ClaimUse Nonce = Maybe (Claim Nonce)
  IdTokenUseF 'ClaimUse TokenCHash = Maybe (Claim TokenCHash)
  IdTokenUseF 'ClaimUse [Acr] = Claim Acr
  IdTokenUseF 'ClaimUse ConsentId = Claim ConsentId

  IdTokenUseF 'TokenUse Nonce = Nonce
  IdTokenUseF 'TokenUse TokenCHash = TokenCHash
  IdTokenUseF 'TokenUse [Acr] = [Acr]
  IdTokenUseF 'TokenUse ConsentId = ConsentId

type family IdTokenMapFunctor (use :: IdTokenUse) where
  IdTokenMapFunctor 'ClaimUse = Claim
  IdTokenMapFunctor 'TokenUse = Identity

type IdTokenClaims = IdToken 'ClaimUse
type IdToken' = IdToken 'TokenUse

instance ToJSON IdTokenClaims where
  toJSON = idTokenToJSON Dict

instance ToJSON IdToken' where
  toJSON = idTokenToJSON Dict

instance FromJSON (IdToken 'ClaimUse) where
  parseJSON v = (withObject "IdToken" $ \o ->
    let
      parseClaim :: FromJSON a => Text -> Parser (Maybe (Claim a))
      parseClaim k =
        traverse parseJSON1 =<< o .:? k
    in
      IdToken
      <$> parseClaim "nonce"
      <*> parseClaim "c_hash"
      <*> (parseJSON1 =<< o .: "acr")
      <*> (parseJSON1 =<< o .: "cdr_consent_id")
      <*> parseJSON v) v

instance FromJSON IdToken' where
  parseJSON = undefined

idTokenToJSON ::
  Dict (
    ToJSON1 (IdTokenMapFunctor g)
  , ToJSON (IdTokenUseF g ConsentId)
  , ToJSON (IdTokenUseF g Nonce)
  , ToJSON (IdTokenUseF g TokenCHash)
  , ToJSON (IdTokenUseF g [Acr])
  )
  -> IdToken g
  -> Value
idTokenToJSON Dict IdToken{..} =
  let
    required = HM.fromList . filter (\(s, m) -> (m /= Null)) $
      [ ("nonce", toJSON idTokenNonce)
      , ("c_hash", toJSON idTokenCHash)
      , ("acr", toJSON idTokenAcr)
      , ("cdr_consent_id", toJSON idTokenConsentId)
      ]
  in
    Object $ required <> toObjectDMap idTokenMap

instance JSONKey IdTokenKey where
  toFieldName = \case
    IdTokenIss      -> "iss"
    IdTokenSub      -> "sub"
    IdTokenAud      -> "aud"
    IdTokenExp      -> "exp"
    IdTokenIat      -> "iat"
    IdTokenAuthTime -> "auth_time"
    IdTokenSHash    -> "s_hash"
    IdTokenAtHash   -> "at_hash"
    IdTokenAddress  -> "address"
    IdTokenPhone    -> "phone_number"
  keys =
    [ This IdTokenIss
    , This IdTokenSub
    , This IdTokenAud
    , This IdTokenExp
    , This IdTokenIat
    , This IdTokenAuthTime
    , This IdTokenSHash
    , This IdTokenAtHash
    , This IdTokenAddress
    , This IdTokenPhone
    ]

instance ToJSON1 f => ToJSON (DMap IdTokenKey f) where
  toJSON = toJSONDMap

instance FromJSON1 f => FromJSON (DMap IdTokenKey f) where
  parseJSON = mkParseJSON "IdTokenMap"

newtype IdTokenIssuer =
  IdTokenIssuer URI
  deriving (Eq, Show)

  {-- additional ID Token fields according to OIDC:
       "name" :"John Doe",
"nickname":"Jimmy",
"preferred_username": "john.doe@example.com",
"given_name":"John",
"middle_name":"James",
"family_name":"Doe",
"profile":"https://example.com/john.doe",
"zoneinfo":"America/Los_Angeles",
"locale":"en-US",
"email":"john.doe@example.com",
"email_verified":true,
"address" : { "street_address": "123 Hollywood Blvd.",
    "locality": "Los Angeles",
    "region": "CA",
    "postal_code": "90210",
    "country": "US"
  },
"phone_number":"+1 (425) 555-1212" --}

deriveGEq ''IdTokenKey
deriveGCompare ''IdTokenKey
deriveGShow ''IdTokenKey

deriveEqTag ''IdTokenKey
deriveShowTag ''IdTokenKey

deriveFromJSONViaKey ''IdTokenKey
deriveToJSONViaKey ''IdTokenKey
