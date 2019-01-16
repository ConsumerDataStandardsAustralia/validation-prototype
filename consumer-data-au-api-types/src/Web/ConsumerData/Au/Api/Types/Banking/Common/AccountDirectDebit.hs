{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDirectDebit
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDirectDebit
  ) where

import           Waargonaut.Decode                       (Decoder)
import qualified Waargonaut.Decode                       as D
import           Waargonaut.Encode                       (Encoder)
import qualified Waargonaut.Encode                       as E
import           Waargonaut.Generic
    (JsonDecode (..), JsonEncode (..))

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.AuthorisedEntity
    (AuthorisedEntity, authorisedEntityDecoder, authorisedEntityEncoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, DateTimeString, amountStringDecoder, amountStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)
import           Web.ConsumerData.Au.Api.Types.Tag


newtype DirectDebitAuthorisations =
  DirectDebitAuthorisations { getAuthorisations :: [AccountDirectDebit] }
    deriving (Eq, Show)

directDebitAuthorisationsDecoder :: Monad f => Decoder f DirectDebitAuthorisations
directDebitAuthorisationsDecoder =
  DirectDebitAuthorisations
    <$> D.atKey "directDebitAuthorisations" (D.list accountDirectDebitDecoder)

directDebitAuthorisationsEncoder :: Applicative f => Encoder f DirectDebitAuthorisations
directDebitAuthorisationsEncoder = E.mapLikeObj $ \dda ->
  E.atKey' "directDebitAuthorisations" (E.list accountDirectDebitEncoder) (getAuthorisations dda)

instance JsonDecode OB DirectDebitAuthorisations where
  mkDecoder = tagOb directDebitAuthorisationsDecoder

instance JsonEncode OB DirectDebitAuthorisations where
  mkEncoder = tagOb directDebitAuthorisationsEncoder


data AccountDirectDebit = AccountDirectDebit
  { _accountDirectDebitAccountId         :: AccountId
  , _accountDirectDebitAuthorisedEntity  :: Maybe AuthorisedEntity
  , _accountDirectDebitLastDebitDateTime :: Maybe DateTimeString
  , _accountDirectDebitLastDebitAmount   :: Maybe AmountString
  } deriving (Eq, Show)

accountDirectDebitDecoder :: Monad f => Decoder f AccountDirectDebit
accountDirectDebitDecoder =
  AccountDirectDebit
    <$> D.atKey "accountId" accountIdDecoder
    <*> atKeyOptional' "authorisedEntity" authorisedEntityDecoder
    <*> atKeyOptional' "lastDebitDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "lastDebitAmount" amountStringDecoder

instance JsonDecode OB AccountDirectDebit where
  mkDecoder = tagOb accountDirectDebitDecoder

accountDirectDebitEncoder :: Applicative f => Encoder f AccountDirectDebit
accountDirectDebitEncoder = E.mapLikeObj $ \ p ->
  E.atKey' "accountId" accountIdEncoder (_accountDirectDebitAccountId p) .
  maybeOrAbsentE "authorisedEntity" authorisedEntityEncoder (_accountDirectDebitAuthorisedEntity p) .
  maybeOrAbsentE "lastDebitDateTime" dateTimeStringEncoder (_accountDirectDebitLastDebitDateTime p) .
  maybeOrAbsentE "lastDebitAmount" amountStringEncoder (_accountDirectDebitLastDebitAmount p)

instance JsonEncode OB AccountDirectDebit where
  mkEncoder = tagOb accountDirectDebitEncoder
