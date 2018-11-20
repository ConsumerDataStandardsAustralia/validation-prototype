{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
  ( module Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
  ) where

import           Data.Functor.Contravariant          ((>$<))
import           Data.Text                           (Text)
import           Data.Time                           (UTCTime)
import           Data.Time.Waargonaut                (utcTimeDecoder, utcTimeEncoder)
import           Servant.API
    (FromHttpApiData,ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode                   (Decoder)
import qualified Waargonaut.Decode                   as D
import           Waargonaut.Encode                   (Encoder)
import qualified Waargonaut.Encode                   as E


-- | All types are from <https://consumerdatastandardsaustralia.github.io/standards/?swagger#common-field-types CDR AU v0.1.0 Common Field Types>

-- | A string representing an amount of currency.
-- - A positive, zero or negative number
-- - Negative numbers identified with a ‘-‘
-- - No currency symbols should be supplied
-- - At least 1 and up to a total of 16 significant digits before decimal point
-- - Minimum 2 digits following a decimal point (more digits allowable but only if required)
-- - No additional formatting, eg thousand separating commas 	“0.01”
-- “10.00”
-- “1234567.89”
-- “-1001.23”
-- “1.999”
data AmountString =
  AmountString { unAmountString :: Text }
  deriving (Show, Eq)

amountStringDecoder :: Monad f => Decoder f AmountString
amountStringDecoder = AmountString <$> D.text

amountStringEncoder :: Applicative f => Encoder f AmountString
amountStringEncoder = unAmountString >$< E.text

-- | Standard UTF-8 string but limited to the ASCII character set.
data AsciiString =
  AsciiString { unAsciiString :: Text }
  deriving (Show, Eq)

asciiStringDecoder :: Monad f => Decoder f AsciiString
asciiStringDecoder = AsciiString <$> D.text

asciiStringEncoder :: Applicative f => Encoder f AsciiString
asciiStringEncoder = unAsciiString >$< E.text

instance ToHttpApiData AsciiString where
  toUrlPiece (AsciiString t) = toUrlPiece t
instance FromHttpApiData AsciiString where
  parseUrlPiece = fmap AsciiString . parseUrlPiece


-- | Standard 3 character currency codes as per ISO-4217 	“AUD”
-- “USD”
-- “GBP”
data CurrencyString =
  CurrencyString { unCurrencyString :: Text}
  deriving (Show, Eq)

currencyStringDecoder :: Monad f => Decoder f CurrencyString
currencyStringDecoder = CurrencyString <$> D.text

currencyStringEncoder :: Applicative f => Encoder f CurrencyString
currencyStringEncoder = unCurrencyString >$< E.text


-- | Date string as per RFC-3339 (labelled full-date in the RFC). UTC time should always be used
-- “2007-05-01”
-- “2012-12-25”
data DateString =
  DateString { unDateString :: Text } 
  deriving (Show, Eq)

dateStringDecoder :: Monad f => Decoder f DateString
dateStringDecoder = DateString <$> D.text

dateStringEncoder :: Applicative f => Encoder f DateString
dateStringEncoder = unDateString >$< E.text


-- | Combined Date and Time string as per RFC-3339 (labelled date-time in the RFC)
-- “2007-05-01T15:43:00.12345Z”
-- “2012-12-25T15:43:00-08:00”
-- “1997-01-12T15:43:00.121Z”
data DateTimeString =
  DateTimeString { unDateTimeString :: UTCTime }
  deriving (Show, Eq)

dateTimeStringDecoder :: Monad f => Decoder f DateTimeString
dateTimeStringDecoder = DateTimeString <$> utcTimeDecoder

dateTimeStringEncoder :: Applicative f => Encoder f DateTimeString
dateTimeStringEncoder = unDateTimeString >$< utcTimeEncoder


-- | Duration (period of time) formatted according to <https://en.wikipedia.org/wiki/ISO_8601#Durations ISO 8601 Durations>
data DurationString =
  DurationString { unDurationString :: Text }
  deriving (Show, Eq)

durationStringDecoder :: Monad f => Decoder f DurationString
durationStringDecoder = DurationString <$> D.text

durationStringEncoder :: Applicative f => Encoder f DurationString
durationStringEncoder = unDurationString >$< E.text


-- | A string representing a percentage interest rate
-- - A positive number (or zero)
-- - At least 1 and up to a total of 16 significant digits before decimal point
-- - Up to 16 digits following the decimal point
-- - No formatting, eg thousand separating commas 	“82”
-- “0.05”
-- “12.3456789”
-- “99.123456789123”
data RateString =
  RateString { unRateString :: Text }
  deriving (Show, Eq)

rateStringDecoder :: Monad f => Decoder f RateString
rateStringDecoder = RateString <$> D.text

rateStringEncoder :: Applicative f => Encoder f RateString
rateStringEncoder = unRateString >$< E.text
