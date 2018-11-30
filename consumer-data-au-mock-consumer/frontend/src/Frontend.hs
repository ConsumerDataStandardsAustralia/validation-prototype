{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import           Control.Lens

import           Data.Attoparsec.ByteString
import           Data.Bifunctor                (Bifunctor, first)
import Data.Functor.Identity (Identity)
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
import qualified           Waargonaut.Decode as D
import qualified           Waargonaut.Encode as E
import           Waargonaut.Decode.Error

import Common.Route
import Obelisk.Generated.Static

data Image = Image
  { _imageWidth    :: Int
  , _imageHeight   :: Int
  , _imageTitle    :: T.Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  } deriving Show

testImage :: Image
testImage = Image 300 600 "Hello World" True [1337]

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text . T.pack . show . decodeBs decodeImage . encodeBs encodeImage $ testImage
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
  }

encodeImage :: Applicative f => E.Encoder f Image
encodeImage = E.mapLikeObj $ \img ->
    E.intAt "Width" (_imageWidth img)
  . E.intAt "Height" (_imageHeight img)
  . E.textAt "Title" (_imageTitle img)
  . E.boolAt "Animated" (_imageAnimated img)
  . E.listAt E.int "IDs" (_imageIDs img)

decodeImage :: Monad f => D.Decoder f Image
decodeImage = D.withCursor $ \curs -> do
  -- Move down into the JSON object.
  io <- D.down curs
  -- We need individual values off of our object,
  Image
    <$> D.fromKey "Width" D.int io
    <*> D.fromKey "Height" D.int io
    <*> D.fromKey "Title" D.text io
    <*> D.fromKey "Animated" D.bool io
    <*> D.fromKey "IDs" (D.list D.int) io

showErr :: (DecodeError, CursorHistory) -> String
showErr (e, ch) = "Waargonaut Decode Failed (" <> show e <> "): " <> show (ppCursorHistory ch)

decodeBs :: D.Decoder Identity a -> BS.ByteString -> Either (DecodeError, CursorHistory) a
decodeBs d = simpleDecode d (over _Left (const $ ParseFailed "") . eitherResult . parse parseWaargonaut)

encodeBs :: E.Encoder' a -> a -> BS.ByteString
encodeBs e = BL.toStrict . runIdentity . E.simpleEncodeNoSpaces e

prefixError :: (Bifunctor p, Semigroup b, IsString b) => b -> p b c -> p b c
prefixError e = first (\x -> e <> " " <> x)
