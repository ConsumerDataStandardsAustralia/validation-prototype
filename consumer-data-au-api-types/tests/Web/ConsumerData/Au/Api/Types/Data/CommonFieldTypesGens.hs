module Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens where

-- import           Data.Text           (Text)
import           Data.Time.Gens      (utcTimeGen)
import           Hedgehog            (MonadGen, Gen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import Web.ConsumerData.Au.Api.Types


amountStringGen :: (MonadGen m) => m AmountString
amountStringGen = AmountString
  <$> Gen.text (Range.linear 5 20) Gen.unicode


asciiStringGen :: (MonadGen m) => m AsciiString
asciiStringGen = AsciiString
  <$> Gen.text (Range.linear 5 20) Gen.ascii


currencyStringGen :: (MonadGen m) => m CurrencyString
currencyStringGen = CurrencyString
  <$> Gen.enumBounded

currencyAmountGen :: Gen CurrencyAmount
currencyAmountGen = CurrencyAmount
  <$> amountStringGen
  <*> Gen.maybe currencyStringGen

dateStringGen :: Gen DateString
dateStringGen = DateString
  <$> Gen.text (Range.linear 5 20) Gen.ascii

dateTimeStringGen :: (MonadGen m) => m DateTimeString
dateTimeStringGen = Gen.lift $ DateTimeString
  <$> utcTimeGen


durationStringGen :: (MonadGen m) => m DurationString
durationStringGen = DurationString
  <$> Gen.text (Range.linear 5 20) Gen.unicode


intGen :: (MonadGen m) => m Int
intGen = Gen.int (Range.linear 0 maxBound)


rateStringGen :: (MonadGen m) => m RateString
rateStringGen = RateString
  <$> Gen.text (Range.linear 5 20) Gen.unicode
