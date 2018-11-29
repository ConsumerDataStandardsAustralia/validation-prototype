{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Person
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Person
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Waargonaut       (utcTimeDecoder, utcTimeEncoder)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)

-- | The individual who authorised the session.
-- <https://consumerdatastandardsaustralia.github.io/standards/?swagger#tocCommonCommonSchemas CDR AU v0.1.0>
data Person = Person
  { _personLastUpdateTime :: UTCTime -- ^ The date and time this this record was last updated.
  , _personFirstName      :: Text
  , _personLastName       :: Text
  , _personMiddleNames    :: [Text]
  , _personPrefix         :: Text -- ^ Title or salutation.
  , _personSuffix         :: Maybe Text -- ^ Used for a trailing suffix to the name.
  , _personOccupationCode :: Maybe OccupationCode -- ^ Value should be a valid <http://www.abs.gov.au/ANZSCO ANZCO v1.2> Standard Occupation classification.
  }
  deriving (Generic, Eq, Show)


-- | Value should be a valid ANZCO v1.2 Standard Occupation classification.
-- http://www.abs.gov.au/ANZSCO
data OccupationCode =
  OccupationCode {getOccupationCode :: Text}
  deriving (Generic, Eq, Show)

occupationCodeDecoder :: Monad m => Decoder m OccupationCode
occupationCodeDecoder = OccupationCode <$> D.text

occupationCodeEncoder :: Monad m => Encoder m OccupationCode
occupationCodeEncoder = getOccupationCode >$< E.text

personEncoder :: Applicative f => Encoder f Person
personEncoder = E.mapLikeObj $ personFields

personFields
  :: (Monoid ws, Semigroup ws)
  => Person -> MapLikeObj ws Json -> MapLikeObj ws Json
personFields p =
  E.atKey' "lastUpdateTime" utcTimeEncoder (_personLastUpdateTime p) .
  E.atKey' "firstName" E.text (_personFirstName p) .
  E.atKey' "lastName" E.text (_personLastName p) .
  E.atKey' "middleNames" (E.list E.text) (_personMiddleNames p) .
  E.atKey' "prefix" E.text (_personPrefix p) .
  maybeOrAbsentE "suffix" E.text (_personSuffix p) .
  maybeOrAbsentE "occupationCode" occupationCodeEncoder (_personOccupationCode p)

personDecoder :: Monad f => Decoder f Person
personDecoder =
  Person
    <$> D.atKey "lastUpdateTime" utcTimeDecoder
    <*> D.atKey "firstName" D.text
    <*> D.atKey "lastName" D.text
    <*> D.atKey "middleNames" (D.list D.text)
    <*> D.atKey "prefix" D.text
    <*> atKeyOptional' "suffix" D.text
    <*> atKeyOptional' "occupationCode" occupationCodeDecoder
