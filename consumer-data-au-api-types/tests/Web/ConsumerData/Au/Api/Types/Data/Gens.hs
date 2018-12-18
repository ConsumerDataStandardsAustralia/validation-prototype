{-# LANGUAGE OverloadedStrings #-}

module Web.ConsumerData.Au.Api.Types.Data.Gens
  (amountStringGen, rateStringGen, asciiStringGen, physicalAddressGen, currencyGen)
where

import           Control.Applicative (liftA3)
import           Data.Currency       (Alpha)
import           Data.Foldable       (Foldable (toList))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Hedgehog            (Gen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import Country.Gens   (countryGen)
import Data.Text.Gens (textGen)

import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
import Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress

-- - A positive, zero or negative number
-- - Negative numbers identified with a '-'
-- - No currency symbols should be supplied
-- - At least 1 and up to a total of 16 significant digits before decimal point
-- - Minimum 2 digits following a decimal point (more digits allowable but only if required)
-- - No additional formatting, eg thousand separating commas  "0.01"
amountStringGen :: Gen AmountString
amountStringGen = AmountString <$> Gen.frequency
  [ (3, positiveRational)
  , (1, positiveInteger)
  , (3, negative positiveRational)
  , (1, negative positiveInteger)
  , (1, pure zero)
  ]

-- | A string representing a percentage interest rate
-- - A positive number (or zero)
-- - At least 1 and up to a total of 16 significant digits before decimal point
-- - Up to 16 digits following the decimal point
-- - No formatting, eg thousand separating commas   “82”
-- “0.05”
-- “12.3456789”
-- “99.123456789123”
rateStringGen :: Gen RateString
rateStringGen = RateString <$> Gen.choice
  [ positiveRational
  , pure zero
  ]

asciiStringGen :: Gen AsciiString
asciiStringGen = AsciiString . Text.pack <$> Gen.list (Range.linear 1 50) Gen.ascii

currencyGen :: Gen Alpha
currencyGen = Gen.enumBounded

physicalAddressGen ::  Gen PhysicalAddress
physicalAddressGen =
  PhysicalAddress
    <$> addressPurposeGen
    <*> addressGen

addressPurposeGen :: Gen AddressPurpose
addressPurposeGen = Gen.element
  [ AddressPurposeRegistered
  , AddressPurposeMail
  , AddressPurposePhysical
  , AddressPurposeWork
  , AddressPurposeOther
  ]

addressGen :: Gen Address
addressGen =  Gen.choice
  [ AddressSimple <$> simpleAddressGen
  , pure AddressPaf
  ]

simpleAddressGen :: Gen SimpleAddress
simpleAddressGen =
  SimpleAddress
    <$> Gen.maybe textGen
    <*> textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> textGen
    <*> addressStateGen
    <*> Gen.maybe countryGen

addressStateGen :: Gen AddressState
addressStateGen = Gen.choice
  [ AustralianState <$> australiaStateGen
  , OtherCountryState <$> textGen
  ]

australiaStateGen :: Gen AustraliaState
australiaStateGen = Gen.element
  [ AustraliaStateACT
  , AustraliaStateNSW
  , AustraliaStateNT
  , AustraliaStateQLD
  , AustraliaStateSA
  , AustraliaStateTAS
  , AustraliaStateVIC
  , AustraliaStateWA
  ]

-- helpers
positiveRational, positiveInteger, postDecimal, dot :: Gen Text
positiveRational = liftA3 (\a b c -> a <> b <> c) positiveInteger dot postDecimal
positiveInteger = toText <$> Gen.nonEmpty (Range.linear 1 16) Gen.digit
postDecimal = toText <$> Gen.nonEmpty (Range.linear 2 16) Gen.digit
dot = pure "."

toText :: Foldable f => f Char -> Text
toText = Text.pack . toList

negative :: Functor f => f Text -> f Text
negative = fmap ("-" <>)

zero :: Text
zero = "0"
