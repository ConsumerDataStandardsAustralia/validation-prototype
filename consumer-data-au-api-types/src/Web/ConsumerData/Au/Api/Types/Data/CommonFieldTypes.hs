{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
  ( module Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
  ) where

import           Control.Lens               (Prism', _Show)
import           Data.Currency              (Alpha)
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text, pack, unpack)
import           Data.Text.Lens             (_Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Format
    (defaultTimeLocale, formatTime, parseTimeM)
import           Data.Time.Waargonaut
    (utcTimeDecoder, utcTimeEncoder, utcTimeFormatString)
import           Servant.API (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E


-- | A string representing an amount of currency.
-- - A positive, zero or negative number
-- - Negative numbers identified with a ‘-‘
-- - No currency symbols should be supplied
-- - At least 1 and up to a total of 16 significant digits before decimal point
-- - Minimum 2 digits following a decimal point (more digits allowable but only if required)
-- - No additional formatting, eg thousand separating commas    “0.01”
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

instance ToHttpApiData AmountString where
  toUrlPiece (AmountString t) = toUrlPiece t
instance FromHttpApiData AmountString where
  parseUrlPiece = fmap AmountString . parseUrlPiece

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


-- | Standard 3 character currency codes as per ISO-4217
-- “AUD”
-- “USD”
-- “GBP”
data CurrencyString =
  CurrencyString { unCurrencyString :: Alpha }
  deriving (Show, Eq)

currencyAlphaText :: Prism' Text Alpha
currencyAlphaText = _Text . _Show

currencyStringDecoder :: Monad f => Decoder f CurrencyString
currencyStringDecoder = CurrencyString <$> D.prismDOrFail
  (D.ConversionFailure "Invalid CurrencyString")
  currencyAlphaText
  D.text

currencyStringEncoder :: Applicative f => Encoder f CurrencyString
currencyStringEncoder = unCurrencyString >$< E.prismE currencyAlphaText E.text


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

instance ToHttpApiData DateTimeString where
  toUrlPiece (DateTimeString t) = pack $ formatTime defaultTimeLocale utcTimeFormatString t
instance FromHttpApiData DateTimeString where
  parseUrlPiece = fmap DateTimeString . parseTimeM True defaultTimeLocale utcTimeFormatString . unpack

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
-- - No formatting, eg thousand separating commas       “82”
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
