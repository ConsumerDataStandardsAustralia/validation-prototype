{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Web.ConsumerData.Au.Api.Types.Auth.AuthApi where

-- import Crypto.JWT                          (SignedJWT)
import Data.Text (Text)
import GHC.Generics                        (Generic)
-- import Servant.API
--     ((:>), QueryParam, QueryParam', Required, StdMethod (GET), Strict, Verb, Get, PlainText)
import Servant.API
    ((:>), QueryParam', Required, StdMethod (GET), Strict, Verb, Get, PlainText)
-- import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic                 ((:-))

-- import Web.ConsumerData.Au.Api.Types.Auth.Common
--     (ClientId, IdToken, IdTokenUse (TokenUse), Nonce, RedirectUri,
--     ResponseType, Scopes, State)

data Foo = Foo

data AuthApi route =
  AuthApi
  {
    -- authorise :: route :- "authorise"
    --   :> RQP "response_type" ResponseType
    --   :> RQP "client_id" ClientId
    --   :> RQP "redirect_uri" RedirectUri
    --   :> RQP "scope" Scopes
    --   :> RQP "request" SignedJWT
    --   :> RQP "nonce" Nonce
    --   :> QueryParam "state" State
    --   :> Get302 '[WaargJSON Foo] (IdToken 'TokenUse)

  inc :: route :- "increment"
      :> Get '[PlainText] Text

  -- , token :: route :- "token"
  --     :> ReqBody '[FormUrlEncoded] TokenRequest
  --     :> Post '[WaargJSON] TokenResponse
  } deriving Generic

-- | Query paramter is required and a failed parse causes an error.
type RQP = QueryParam' '[Required, Strict]

type Get302 = Verb 'GET 302
