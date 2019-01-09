{-# LANGUAGE DataKinds #-}

module Web.ConsumerData.Au.LambdaBank.Server.Auth where

import Crypto.JWT             (SignedJWT)
import Data.Text (Text, pack)
import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.Api.Types
    (AuthApi (..), ClientId, IdToken, Nonce, RedirectUri, ResponseType, Scopes,
    State, IdTokenUse (TokenUse))
import Web.ConsumerData.Au.LambdaBank.AuthModel       (incrementCount)
import Web.ConsumerData.Au.LambdaBank.Server.Internal (LambdaBankM)

authServer :: ToServant AuthApi (AsServerT LambdaBankM)
authServer = genericServerT AuthApi
  { -- authorise = authoriseServer
    inc = incServer
  }

authoriseServer ::
  ResponseType
  -> ClientId
  -> RedirectUri
  -> Scopes
  -> SignedJWT
  -> Nonce
  -> Maybe State
  -> LambdaBankM (IdToken 'TokenUse)
authoriseServer =
  error "TODO: implement authorise server"

incServer ::
  LambdaBankM Text
incServer = pack . show <$> incrementCount
