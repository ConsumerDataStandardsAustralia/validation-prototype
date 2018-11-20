{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}

module Web.ConsumerData.Au.Api.Types.Auth.Common.AccessToken where

import Data.Text (Text)

-- Access Tokens may be opaque strings or JWTs depending depending on impls
type AccessToken = Text
data TokenTypeDescription = Bearer --"Bearer"

{--
  --TODO: the access token response differs with auth code + private_key_jwt
  -- mandatory headers for the access token response
  data AccessTokenHeaders = AccessTokenHeaders
    {
       _alg :: "RS256"
      ,_kid :: KeyId
    }

  data OptionalAccessTokenHeaders = OptionalAccessTokenHeaders { }

  data AccessTokenPayload = AccessTokenPayload
   {
      _iss :: Iss
    , _sub :: TokenSubject
    ,  _exp :: TokenExpiry
    ,  _iat :: TokenAccessTime
    ,  _jti :: TokenIdentifier
    ,  _aud :: TokenAudience
   }
--}

 {-- TODO: confirm where to use JOSE header components:

      ,_iss :: Iss
      ,_iat :: Iat
      -- Critical elements required
      ,_crit :: [Crit]
      ,_b64 :: "false"
      , _typ :: Maybe "JOSE"
      , _cty :: Maybe Cty

  -- `crit` elements supplied as mandatory headers, required to be JSON list
  criticalElements = ["b64", "http://openbanking.org.uk/iat", "http://openbanking.org.uk/iss"] :: [Crit]

  data Cty = "json" | "application/json"
  newtype Crit = Text

 --}


