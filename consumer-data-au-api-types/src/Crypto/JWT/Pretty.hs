{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Crypto.JWT.Pretty
  ( PrettyJwt
  , mkPrettyJwt
  , removePart
  , JwtPart (..)
  , PrettyJwtError
  , AsPrettyJwtError (..)
  ) where

import           Control.Lens
    (makeClassyPrisms, preview, ( # ))
import           Control.Monad.Error.Class (MonadError, throwError)
import           Crypto.JOSE               (encodeCompact)
import           Crypto.JOSE.Types         (base64url)
import           Crypto.JWT                (SignedJWT)
import           Data.Aeson
    (FromJSON, Object, ToJSON, Value (String), decodeStrict, (.=))
import qualified Data.ByteString.Lazy      as BS
import           Data.Char                 (ord)
import qualified Data.HashMap.Strict       as HM
import           Data.Text                 (Text)
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
  PrettyJwt Object
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
    lHpsB64 = zip [Header, Payload, Signature] hpsB64
    decode e f =
      traverse (\(part, a) -> maybe (throwError $ e # part) (pure . (part,)) $ f a)

  -- Transform the header and payload to aeson @Object@s
  hpsBS <- decode _Base64DecodeError (preview base64url) (take 2 lHpsB64)
  (hpJSON :: [(JwtPart, Value)]) <- decode _AesonDecodeError decodeStrict hpsBS

  -- Transform the base64 signature to an aeson @String@
  sJSON <- decode _AesonDecodeError (pure . String . decodeUtf8 . BS.toStrict) $ drop 2 lHpsB64

  case fmap (\(p,json) -> prettyPart p .= json) (hpJSON <> sJSON) of
    kvs@[_,_,_] ->
      pure . PrettyJwt . HM.fromList $ kvs
    _ -> throwError $ _NotExactlyThreeElements # ()

removePart ::
  JwtPart
  -> PrettyJwt
  -> PrettyJwt
removePart p (PrettyJwt o) =
  PrettyJwt $ HM.delete (prettyPart p) o

prettyPart ::
  JwtPart
  -> Text
prettyPart = \case
  Header -> "header"
  Payload -> "payload"
  Signature -> "signature"

