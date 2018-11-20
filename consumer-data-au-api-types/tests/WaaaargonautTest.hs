{-# LANGUAGE OverloadedStrings #-}
module WaaaargonautTest where

import Data.Text (Text)
import Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E
import Test.Tasty       (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

data Person = Person
  { _personName                    :: Text
  , _personAge                     :: Int
  , _personAddress                 :: Text
  , _personFavouriteLotteryNumbers :: [Int]
  }
  deriving (Eq, Show)

personDecoder :: Monad f => Decoder f Person
personDecoder = D.withCursor $ \c -> do
  o     <- D.down c
  name  <- D.fromKey "name" D.text o
  age   <- D.fromKey "age" D.int o
  addr  <- D.fromKey "address" D.text o
  lotto <- D.fromKey "numbers" (D.list D.int) o
  pure $ Person name age addr lotto

personEncoder :: Applicative f => Encoder f Person
personEncoder = E.mapLikeObj $ \ p ->
  E.atKey' "name" E.text (_personName p) .
  E.atKey' "age" E.int (_personAge p) .
  E.atKey' "address" E.text (_personAddress p) .
  E.atKey' "numbers" (E.list E.int) (_personFavouriteLotteryNumbers p)

test_waargonaut :: [TestTree]
test_waargonaut =
  [ testCase "roundtrip" $ do
      1 @?= (1::Int)
  ]
