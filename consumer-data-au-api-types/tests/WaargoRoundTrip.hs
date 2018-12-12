{-# LANGUAGE OverloadedStrings #-}
module WaargoRoundTrip where

import           Control.Lens
import           Control.Monad              ((>=>))
import           Control.Monad.Except
    (ExceptT (ExceptT), liftIO, runExceptT)
import           Data.Aeson                 (Value, eitherDecode)
import           Data.Aeson.Diff            (diff)
import           Data.Attoparsec.ByteString
import           Data.Bifunctor             (first)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Either                (either)
import           Data.String                (IsString)
import           Hedgehog
import           Test.Tasty                 (TestName, TestTree)
import           Test.Tasty.Hedgehog        (testProperty)
import           Test.Tasty.HUnit           (testCase, (@?=))
import           Waargonaut
import           Waargonaut.Decode
    (CursorHistory, Decoder, ppCursorHistory, simpleDecode)
import           Waargonaut.Decode.Error
import           Waargonaut.Encode          (Encoder', simpleEncodeNoSpaces)

roundTripProp :: (Show a, Eq a) => Gen a -> Decoder Identity a -> Encoder' a -> TestName -> TestTree
roundTripProp gen d e tn = testProperty tn . property $ do
  a <- forAll gen
  tripping a (encodeBs e) (decodeBs d)

roundTripTest :: Decoder Identity a -> Encoder' a -> TestName -> FilePath -> TestTree
roundTripTest d e tn gp = testCase tn . run $ do
    goldenVal <- ExceptT $ prefixError "Couldn't decode value from golden file:" . decodeValue <$> BL.readFile gp
    decodeRes <- ExceptT $ first showErr . (decodeBs d) <$> BS.readFile gp
    expectedVal <- ExceptT . pure $ prefixError "Aeson parsing of waargonaut output failed:" . decodeValue . BL.fromStrict . encodeBs e $ decodeRes
    let ddiff = diff goldenVal expectedVal
    liftIO $ ddiff @?= mempty
  where
    run = runExceptT >=> either error pure

showErr :: (DecodeError, CursorHistory) -> String
showErr (e, ch) = "Waargonaut Decode Failed (" <> show e <> "): " <> show (ppCursorHistory ch)

decodeBs :: Decoder Identity a -> BS.ByteString -> Either (DecodeError, CursorHistory) a
decodeBs d = simpleDecode d (over _Left (const $ ParseFailed "") . eitherResult . parse parseWaargonaut)

encodeBs :: Encoder' a -> a -> BS.ByteString
encodeBs e = BL.toStrict . runIdentity . simpleEncodeNoSpaces e

decodeValue :: BL.ByteString -> Either String Value
decodeValue = eitherDecode

prefixError :: (Bifunctor p, Semigroup b, IsString b) => b -> p b c -> p b c
prefixError e = first (\x -> e <> " " <> x)
