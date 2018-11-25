module Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens where

import           Data.Text           (Text)
import           Data.Time.Gens      (utcTimeGen)
import           Hedgehog            (MonadGen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import Web.ConsumerData.Au.Api.Types


-- amountStringGen :: Gen AmountString
amountStringGen :: (MonadGen m) => m AmountString
amountStringGen = AmountString
  <$> Gen.text (Range.linear 5 20) Gen.unicode


-- asciiStringGen :: Gen AsciiString
asciiStringGen :: (MonadGen m) => m AsciiString
asciiStringGen = AsciiString
  <$> Gen.text (Range.linear 5 20) Gen.ascii


-- currencyStringGen :: Gen CurrencyString
currencyStringGen :: (MonadGen m) => m CurrencyString
currencyStringGen = CurrencyString
  <$> Gen.text (Range.linear 5 20) Gen.unicode


-- currencyStringGen :: Gen CurrencyString
dateTimeStringGen :: (MonadGen m) => m DateTimeString
dateTimeStringGen = Gen.lift $ DateTimeString
  <$> utcTimeGen


-- durationStringGen :: Gen DurationString
durationStringGen :: (MonadGen m) => m DurationString
durationStringGen = DurationString
  <$> Gen.text (Range.linear 5 20) Gen.unicode


-- intGen :: Gen Int
intGen :: (MonadGen m) => m Int
intGen = Gen.int (Range.linear 0 maxBound)


-- rateStringGen :: Gen RateString
rateStringGen :: (MonadGen m) => m RateString
rateStringGen = RateString
  <$> Gen.text (Range.linear 5 20) Gen.unicode

-- textGen :: Gen Text
textGen :: (MonadGen m) => m Text
textGen = Gen.text (Range.linear 5 20) Gen.unicode
