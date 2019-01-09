-- | A newtype for Servant and other instances to hang off.
module Crypto.JWT.Cdr where

import Crypto.JOSE.Compact     (encodeCompact)
import Crypto.JWT              (SignedJWT)
import Data.Text.Lazy          (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Servant.API             (ToHttpApiData (toQueryParam))

newtype CdrJwt =
  CdrJwt { getSignedJwt :: SignedJWT}
  deriving (Eq, Show)

instance ToHttpApiData CdrJwt where
  toQueryParam =
    toStrict . decodeUtf8 . encodeCompact . getSignedJwt
