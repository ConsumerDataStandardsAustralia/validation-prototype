{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Crypto.JWT.Pretty
  ( PrettyJWT
  , mkPrettyJWT
  ) where

import           Control.Lens         (preview)
import           Crypto.JOSE          (encodeCompact)
import           Crypto.JOSE.Types    (base64url)
import           Crypto.JWT           (SignedJWT)
import           Data.Aeson           (FromJSON, ToJSON, Value, decodeStrict, object, (.=))
import qualified Data.ByteString.Lazy as BS
import           Data.Char            (ord)
import           Data.Maybe           (mapMaybe)

newtype PrettyJWT =
  PrettyJWT Value
  deriving (Eq, Show, ToJSON, FromJSON)

mkPrettyJWT ::
  SignedJWT
  -> PrettyJWT
mkPrettyJWT jwt =
  let
    -- The signing process uses random inputs, so just verify the header and payload hasn't
    -- changed. Round tripping ensures that the signatures are valid.
    dot = fromIntegral . ord $ '.'
    hpsB64 = take 2 . BS.split dot . encodeCompact $ jwt
    hpsBS = mapMaybe (preview base64url) hpsB64
    hpsJSON :: [Value]
    hpsJSON = mapMaybe decodeStrict hpsBS
  in
    PrettyJWT . object . zipWith (.=) ["header", "payload"] $ hpsJSON

