{-# LANGUAGE DataKinds #-}

module Web.ConsumerData.Au.LambdaBank.Server.Auth where

import Crypto.JWT (SignedJWT)
import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.LambdaBank.Server.Internal         (LambdaBankM)

authServer :: ToServant AuthApi (AsServerT LambdaBankM)
authServer = genericServerT AuthApi
  { authorise = authoriseServer }

authoriseServer ::
  ResponseType
  -> ClientId
  -> RedirectUri
  -> Scopes
  -> SignedJWT
  -> Nonce
  -> Maybe State
  -> LambdaBankM (IdToken 'TokenUse)
authoriseServer clientId redirectUri scopes signedJwt nonce mstate =
  -- scopes validated by FromJSON instance
  -- required parameters handled by servant
  -- TODO: check redirectUri validated by smart constructor in Text.URI
  -- Ensure query param values match JWT values
  -- Ensure client id is valid
  error "TODO: implement authorise server"
