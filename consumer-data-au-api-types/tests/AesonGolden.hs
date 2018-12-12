{-# LANGUAGE ScopedTypeVariables #-}

module AesonGolden where

import           Control.Lens         (view)
import           Data.Aeson
    (FromJSON, ToJSON, decodeStrict, encode, toJSON)
import           Data.Aeson.Diff      (diff, patchOperations)
import Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool            (bool)
import qualified Data.ByteString      as BS
import           Data.ByteString.Lens (unpackedChars)

import Test.Tasty                 (TestName, TestTree)
import Test.Tasty.Golden.Advanced (goldenTest)

aesonGolden ::
  forall a.
  ( FromJSON a
  , ToJSON a
  )
  => TestName
  -> FilePath
  -> IO a
  -> TestTree
aesonGolden name gf a =
  let
    bsToVal =
      maybe (error "Unable to decode golden file") pure . decodeStrict
    readGolden =
      bsToVal =<< BS.readFile gf
  in
    goldenTest
      name
      readGolden
      a
      aesonDiff
      (writeFile gf . view unpackedChars . encodePretty)

aesonDiff ::
  ToJSON a
  => a
  -> a
  -> IO (Maybe String)
aesonDiff v1 v2 =
  let
    patch = diff (toJSON v1) (toJSON v2)
    hasPatch = null . patchOperations $ patch
    prettyPatch = Just . view unpackedChars . encode $ patch
  in
    pure $ bool prettyPatch Nothing hasPatch
