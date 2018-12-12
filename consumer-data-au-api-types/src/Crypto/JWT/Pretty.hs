{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Crypto.JWT.Pretty
  ( PrettyJwt
  , mkPrettyJwt
  , PrettyJwtError
  , AsPrettyJwtError (..)
  ) where

import           Control.Lens              (makeClassyPrisms, preview, ( # ))
import           Control.Monad             (zipWithM)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Crypto.JOSE               (encodeCompact)
import           Crypto.JOSE.Types         (base64url)
import           Crypto.JWT                (SignedJWT)
import           Data.Aeson
    (FromJSON, ToJSON, Value (String), decodeStrict, object, (.=))
import qualified Data.ByteString.Lazy      as BS
import           Data.Char                 (ord)
import           Data.Text.Encoding        (decodeUtf8)

data JwtPart =
  Header
  | Payload
  | Signature
  deriving (Eq, Show)

data PrettyJwtError =
  NotExactlyThreeElements
  | Base64DecodeError JwtPart
  | AesonDecodeError JwtPart
  deriving (Eq, Show)

makeClassyPrisms ''PrettyJwtError

newtype PrettyJwt =
  PrettyJwt Value
  deriving (Eq, Show, ToJSON, FromJSON)

mkPrettyJwt ::
  ( AsPrettyJwtError e
  , MonadError e m
  )
  => SignedJWT
  -> m PrettyJwt
mkPrettyJwt jwt = do
  let
    dot = fromIntegral . ord $ '.'
    hpsB64 = BS.split dot . encodeCompact $ jwt
    parts = [Header, Payload, Signature]
    decode e f =
      zipWithM (\part -> maybe (throwError $ e # part) pure . f)

  hpsBS <- decode _Base64DecodeError (preview base64url) parts hpsB64
  (hpJSON :: [Value]) <- decode _AesonDecodeError decodeStrict (take 2 parts) (take 2 hpsBS)
  sJSON <- decode _AesonDecodeError (pure . String . decodeUtf8 . BS.toStrict) [Signature] $ drop 2 hpsB64
  case zipWith (.=) ["header", "payload", "signature"] (hpJSON <> sJSON) of
    kvs@[_,_,_] ->
      pure . PrettyJwt . object $ kvs
    _ -> throwError $ _NotExactlyThreeElements # ()
