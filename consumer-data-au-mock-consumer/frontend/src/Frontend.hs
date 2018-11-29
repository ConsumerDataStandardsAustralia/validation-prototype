{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import Control.Lens

import           Data.Attoparsec.ByteString
import           Data.Bifunctor                (first)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.String                   (IsString)
import qualified Data.Text                     as T
import           Obelisk.Frontend
import           Obelisk.Route
import           Reflex.Dom.Core
import           Waargonaut
import           Waargonaut.Decode
    (CursorHistory, Decoder, ppCursorHistory, simpleDecode)
import           Waargonaut.Decode.Error
import           Waargonaut.Encode             (Encoder', simpleEncodeNoSpaces)
import           Web.ConsumerData.Au.Api.Types

import Common.Route
import Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text . T.pack . show . decodeBs physicalAddressDecoder . encodeBs physicalAddressEncoder $ testAddress
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
  }

testAddress :: PhysicalAddress
testAddress = PhysicalAddress AddressPurposeRegistered . AddressSimple $ SimpleAddress
   (Just "Ben")
   "8 Duncan Street"
   Nothing
   Nothing
   (Just "4006")
   "Brisbane"
   (AustralianState AustraliaStateQLD)
   Nothing

showErr :: (DecodeError, CursorHistory) -> String
showErr (e, ch) = "Waargonaut Decode Failed (" <> show e <> "): " <> show (ppCursorHistory ch)

decodeBs :: Decoder Identity a -> BS.ByteString -> Either (DecodeError, CursorHistory) a
decodeBs d = simpleDecode d (over _Left (const $ ParseFailed "") . eitherResult . parse parseWaargonaut)

encodeBs :: Encoder' a -> a -> BS.ByteString
encodeBs e = BL.toStrict . runIdentity . simpleEncodeNoSpaces e

prefixError :: (Bifunctor p, Semigroup b, IsString b) => b -> p b c -> p b c
prefixError e = first (\x -> e <> " " <> x)
