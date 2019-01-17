{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Person
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Person
  ) where

import           Data.Digit.Decimal
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Waargonaut       (utcTimeDecoder, utcTimeEncoder)
import           Data.Vector.V6
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)


data Person = Person
  { _personLastUpdateTime :: UTCTime
  , _personFirstName      :: Maybe Text
  , _personLastName       :: Text
  , _personMiddleNames    :: [Text]
  , _personPrefix         :: Maybe Text
  , _personSuffix         :: Maybe Text
  , _personOccupationCode :: Maybe OccupationCode
  }
  deriving (Generic, Eq, Show)

-- | Value should be a valid ANZCO v1.2 Standard Occupation classification.
-- http://www.abs.gov.au/ANZSCO
data OccupationCode =
  OccupationCode { getOccupationCode :: V6 DecDigit }
  deriving (Generic, Eq, Show)

occupationCodeDecoder :: Monad m => Decoder m OccupationCode
occupationCodeDecoder = OccupationCode <$> v6DigitDecoder

occupationCodeEncoder :: Monad m => Encoder m OccupationCode
occupationCodeEncoder = getOccupationCode >$< v6DigitEncoder

personEncoder :: Applicative f => Encoder f Person
personEncoder = E.mapLikeObj $ personFields

personFields
  :: (Monoid ws, Semigroup ws)
  => Person -> MapLikeObj ws Json -> MapLikeObj ws Json
personFields p =
  E.atKey' "lastUpdateTime" utcTimeEncoder (_personLastUpdateTime p) .
  maybeOrAbsentE "firstName" E.text (_personFirstName p) .
  E.atKey' "lastName" E.text (_personLastName p) .
  E.atKey' "middleNames" (E.list E.text) (_personMiddleNames p) .
  maybeOrAbsentE "prefix" E.text (_personPrefix p) .
  maybeOrAbsentE "suffix" E.text (_personSuffix p) .
  maybeOrAbsentE "occupationCode" occupationCodeEncoder (_personOccupationCode p)

personDecoder :: Monad f => Decoder f Person
personDecoder =
  Person
    <$> D.atKey "lastUpdateTime" utcTimeDecoder
    <*> atKeyOptional' "firstName" D.text
    <*> D.atKey "lastName" D.text
    <*> D.atKey "middleNames" (D.list D.text)
    <*> atKeyOptional' "prefix" D.text
    <*> atKeyOptional' "suffix" D.text
    <*> atKeyOptional' "occupationCode" occupationCodeDecoder
