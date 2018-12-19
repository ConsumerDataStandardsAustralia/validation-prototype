module Web.ConsumerData.Au.LambdaBank.Server.State where

import           Crypto.JOSE.JWK (JWK)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Text       (Text)

import Web.ConsumerData.Au.Api.Types
    (ClientId, FapiPermittedAlg, LinkQualifier)
import Web.ConsumerData.Au.Api.Types.Auth.Registration (ClientSecret)

-- | Server's state for each DR.
data Client =
  Client
  { _clientSecret                  :: ClientSecret
  , _clientName                    :: Text
  , _clientRequestObjectSigningAlg :: FapiPermittedAlg
  }

-- | Lambda Bank application state. Parameterised on a connection type as it
-- will change depending on how state is managed.
data LbState conn =
  LbState
  { _lbStateConn          :: conn
  , _lbStateLinkQualifier :: LinkQualifier
  }
