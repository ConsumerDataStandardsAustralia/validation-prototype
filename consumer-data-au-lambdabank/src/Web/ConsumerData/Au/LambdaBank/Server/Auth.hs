{-# LANGUAGE DataKinds #-}

module Web.ConsumerData.Au.LambdaBank.Server.Auth where

import Crypto.JWT             (SignedJWT)
import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.Api.Types.Auth
    (AuthApi (AuthApi), ClientId, IdToken, Nonce, Prompt, RedirectUri, ResponseType,
    Scopes, State, IdTokenUse (TokenUse), authorise)
import Web.ConsumerData.Au.LambdaBank.Server.Internal (LambdaBankM)

authServer :: ToServant AuthApi (AsServerT LambdaBankM)
authServer = genericServerT AuthApi
  { authorise = authoriseServer }

authoriseServer ::
  ResponseType
  -> ClientId
  -> RedirectUri
  -> Scopes
  -> Nonce
  -> State
  -> Prompt
  -> SignedJWT
  -> LambdaBankM (IdToken 'TokenUse)
authoriseServer clientId redirectUri scopes nonce state prompt signedJwt =
  -- scopes validated by FromJSON instance
  -- required parameters handled by servant
  -- TODO: check redirectUri validated by smart constructor in Text.URI
  -- Ensure query param values match JWT values
  -- Ensure client id is valid

  error "TODO: implement authorise server"

checkQueryParamsMatchJwt ::
  ResponseType
  -> ClientId
  -> RedirectUri
  -> Scopes
  -> SignedJWT
  -> Nonce
  -> Maybe State
  -> Bool
checkQueryParamsMatchJwt =
  error "TODO: implement checkQueryParamsMatchJwt"
