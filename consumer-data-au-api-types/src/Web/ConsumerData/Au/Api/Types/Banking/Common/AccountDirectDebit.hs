{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE RankNTypes        #-}
-- {-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDirectDebit
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDirectDebit
  ) where

import           Waargonaut.Decode                       (Decoder)
import qualified Waargonaut.Decode                       as D
import           Waargonaut.Encode                       (Encoder)
import qualified Waargonaut.Encode                       as E
import           Waargonaut.Generic
    (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.AuthorisedEntity
    (AuthorisedEntity, authorisedEntityDecoder, authorisedEntityEncoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, DateTimeString, amountStringDecoder, amountStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)
import           Web.ConsumerData.Au.Api.Types.Tag


-- | AccountDirectDebit <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaaccountdirectdebit CDR AU v0.1.0 AccountDirectDebit>
data AccountDirectDebit = AccountDirectDebit
  { _accountDirectDebitAccountId         :: AccountId -- ^ A unique ID of the account adhering to the standards for ID permanence.
  , _accountDirectDebitAuthorisedEntity  :: Maybe AuthorisedEntity
  , _accountDirectDebitLastDebitDateTime :: Maybe DateTimeString -- ^ The date and time of the last debit executed under this authorisation
  , _accountDirectDebitLastDebitAmount   :: Maybe AmountString
  } deriving (Eq, Show)

accountDirectDebitDecoder :: Monad f => Decoder f AccountDirectDebit
accountDirectDebitDecoder = D.withCursor $ \c -> do
  o <- D.down c
  accId <- D.fromKey "accountId" accountIdDecoder o
  authEntity <- D.fromKey "authorisedEntity" (D.maybeOrNull authorisedEntityDecoder) o
  lastDebitDT <- D.fromKey "lastDebitDateTime" (D.maybeOrNull dateTimeStringDecoder) o
  amount <- D.fromKey "lastDebitAmount" (D.maybeOrNull amountStringDecoder) o
  pure $ AccountDirectDebit accId authEntity lastDebitDT amount

instance JsonDecode OB AccountDirectDebit where
  mkDecoder = tagOb accountDirectDebitDecoder

accountDirectDebitEncoder :: Applicative f => Encoder f AccountDirectDebit
accountDirectDebitEncoder = E.mapLikeObj $ \ p ->
  E.atKey' "accountId" accountIdEncoder (_accountDirectDebitAccountId p) .
  E.atKey' "authorisedEntity" (E.maybeOrNull authorisedEntityEncoder) (_accountDirectDebitAuthorisedEntity p) .
  E.atKey' "lastDebitDateTime" (E.maybeOrNull dateTimeStringEncoder) (_accountDirectDebitLastDebitDateTime p) .
  E.atKey' "lastDebitAmount" (E.maybeOrNull amountStringEncoder) (_accountDirectDebitLastDebitAmount p)

instance JsonEncode OB AccountDirectDebit where
  mkEncoder = tagOb accountDirectDebitEncoder

newtype DirectDebitAuthorisations
  = DirectDebitAuthorisations { getAuthorisations :: [AccountDirectDebit] }

directDebitAuthorisationsDecoder :: Monad f => Decoder f DirectDebitAuthorisations
directDebitAuthorisationsDecoder = D.withCursor $ \c -> do
  o <- D.down c
  auths <- D.fromKey "directDebitAuthorisations" (D.list accountDirectDebitDecoder) o
  pure $ DirectDebitAuthorisations auths

directDebitAuthorisationsEncoder :: Applicative f => Encoder f DirectDebitAuthorisations
directDebitAuthorisationsEncoder = E.mapLikeObj $ \dda ->
  E.atKey' "directDebitAuthorisations" (E.list accountDirectDebitEncoder) (getAuthorisations dda)

instance JsonDecode OB DirectDebitAuthorisations where
  mkDecoder = tagOb directDebitAuthorisationsDecoder

instance JsonEncode OB DirectDebitAuthorisations where
  mkEncoder = tagOb directDebitAuthorisationsEncoder
