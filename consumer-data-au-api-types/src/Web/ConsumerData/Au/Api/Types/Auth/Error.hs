{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.ConsumerData.Au.Api.Types.Auth.Error where

import           Control.Lens        (makeClassyPrisms, prism)
import           Control.Monad.Catch (Exception)
import qualified Crypto.JOSE.Error   as JE
import           Crypto.JWT          (AsJWTError (..), JWTError)
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

makeClassyPrisms ''Error
