{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Web.ConsumerData.Au.Api.Types.Auth.OpApi where

import Crypto.JWT                          (SignedJWT)
import Servant.API
    ((:>), FormUrlEncoded, Post, QueryParam, QueryParam', ReqBody, Required,
    StdMethod (GET), Strict, Verb)
import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic                 ((:-))

import Web.ConsumerData.Au.Api.Types.Auth.Common
    (ClientId, IdToken, Nonce, RedirectUri, ResponseType, Scopes, State, IdTokenUse (TokenUse))

data Foo = Foo

data OpApi route =
  OpApi
  {
    authorise :: route :- "authorise"
      :> RQP "response_type" ResponseType
      :> RQP "client_id" ClientId
      :> RQP "redirect_uri" RedirectUri
      :> RQP "scope" Scopes
      :> RQP "request" SignedJWT
      :> RQP "nonce" Nonce
      :> QueryParam "state" State
      :> Get302 '[WaargJSON Foo] (IdToken 'TokenUse)

  -- , token :: route :- "token"
  --     :> ReqBody '[FormUrlEncoded] TokenRequest
  --     :> Post '[WaargJSON] TokenResponse
  }

-- | Query paramter is required and a failed parse causes an error.
type RQP = QueryParam' '[Required, Strict]

type Get302 = Verb 'GET 302
