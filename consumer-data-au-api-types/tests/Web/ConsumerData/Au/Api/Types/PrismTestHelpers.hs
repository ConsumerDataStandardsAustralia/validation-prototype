{-# LANGUAGE RankNTypes #-}
module Web.ConsumerData.Au.Api.Types.PrismTestHelpers where

import Control.Lens

import           Hedgehog            (property, forAll, tripping)
import qualified Hedgehog.Gen        as Gen
import           Test.Tasty          (TestName, TestTree)
import           Test.Tasty.Hedgehog (testProperty)

testEnumPrismTripping :: (Enum a, Bounded a, Show a, Eq a, Show b) => TestName -> Prism' b a -> TestTree
testEnumPrismTripping tn p = testProperty tn . property $ do
  a  <- forAll Gen.enumBounded
  tripping a (p #) (^? p)
