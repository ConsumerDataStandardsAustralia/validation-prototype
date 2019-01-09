module Data.Text.Gens where

import Data.Text (Text)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

textGen :: (MonadGen m) => m Text
textGen = Gen.text (Range.linear 5 20) Gen.unicode
