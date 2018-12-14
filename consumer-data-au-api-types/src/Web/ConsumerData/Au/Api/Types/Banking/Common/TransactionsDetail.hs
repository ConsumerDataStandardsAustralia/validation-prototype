{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionsDetail
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionsDetail
  ) where

import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import           Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import           Waargonaut.Encode (Encoder')
import qualified Waargonaut.Encode as E

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionExtendedData
    (TransactionExtendedData, transactionExtendedDataDecoder,
    transactionExtendedDataEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
    (TransactionId, TransactionStatus, transactionIdDecoder,
    transactionIdEncoder, transactionStatusDecoder, transactionStatusEncoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, CurrencyString, DateTimeString, amountStringDecoder,
    amountStringEncoder, currencyStringDecoder, currencyStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)


data TransactionsDetail = TransactionsDetail
  { _transactionsDetailAccountId    :: AccountId
  , _transactionsDetailDisplayName  :: Text
  , _transactionsDetailNickname     :: Maybe Text
  , _transactionsDetailTransactions :: Transactions
  } deriving (Eq, Show)

transactionsDetailDecoder :: Monad f => Decoder f TransactionsDetail
transactionsDetailDecoder =
  TransactionsDetail
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey "transactions" transactionDetailsDecoder

transactionsDetailEncoder :: Applicative f => Encoder f TransactionsDetail
transactionsDetailEncoder = E.mapLikeObj $ \(TransactionsDetail accId dName nName ts) ->
  E.atKey' "accountId" accountIdEncoder accId .
  E.atKey' "displayName" E.text dName .
  maybeOrAbsentE "nickname" E.text nName .
  E.atKey' "transactions" transactionDetailsEncoder ts
