{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.ConsumerData.Au.Api.Types.Auth.Error where

import           Control.Lens        (makeClassyPrisms, prism)
import           Control.Monad.Catch (Exception)
import qualified Crypto.JOSE.Error   as JE
import           Crypto.JWT          (AsJWTError (..), JWTError)
import           Crypto.JWT.Pretty
    (AsPrettyJwtError (_PrettyJwtError), PrettyJwtError)
import           Data.Text           (Text)

data Error =
  MissingClaim Text
  | InvalidClaim Text
  | ParseError String
  | JoseError JE.Error
  | JoseJwtError JWTError
  deriving (Show, Eq)


data HttpsUrlError =
  NotHttps
  | MissingScheme
  | UriParseError
  deriving (Eq, Show)

instance Exception HttpsUrlError

makeClassyPrisms ''HttpsUrlError


instance JE.AsError Error where
  _Error =
    prism JoseError $
          \case JoseError e -> Right e
                e -> Left e

instance AsJWTError Error where
  _JWTError =
    prism JoseJwtError $
          \case JoseJwtError e -> Right e
                e -> Left e

data GoldenError = InvalidGoldenE | AuthE Error | PrettyJwtE PrettyJwtError deriving (Eq, Show)

makeClassyPrisms ''GoldenError

newtype JwtFailure = JwtFailure String
  deriving (Eq, Show)

instance Exception JwtFailure

instance AsPrettyJwtError GoldenError where
  _PrettyJwtError = _PrettyJwtE . _PrettyJwtError

makeClassyPrisms ''Error

instance AsError GoldenError where
  _Error = _AuthE . _Error

instance JE.AsError GoldenError where
  _Error = _Error . _JoseError
