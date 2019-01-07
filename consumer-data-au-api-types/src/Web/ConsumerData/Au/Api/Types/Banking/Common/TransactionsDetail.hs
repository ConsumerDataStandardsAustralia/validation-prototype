{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionsDetail
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionsDetail
  ) where

import           Data.Text          (Text)
import           Waargonaut.Decode  (Decoder)
import qualified Waargonaut.Decode  as D
import           Waargonaut.Encode  (Encoder)
import qualified Waargonaut.Encode  as E
import           Waargonaut.Generic (JsonDecode (..), JsonEncode (..))

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionDetail
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


data TransactionsDetail = TransactionsDetail
  { _transactionsDetailAccountId    :: AccountId
  , _transactionsDetailDisplayName  :: Text
  , _transactionsDetailNickname     :: Maybe Text
  , _transactionsDetailTransactions :: TransactionDetails
  } deriving (Eq, Show)

instance JsonDecode OB TransactionsDetail where
  mkDecoder = tagOb transactionsDetailDecoder

instance JsonEncode OB TransactionsDetail where
  mkEncoder = tagOb transactionsDetailEncoder

transactionsDetailDecoder :: Monad f => Decoder f TransactionsDetail
transactionsDetailDecoder =
  TransactionsDetail
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey "transaction" transactionDetailsDecoder

transactionsDetailEncoder :: Applicative f => Encoder f TransactionsDetail
transactionsDetailEncoder = E.mapLikeObj $ \(TransactionsDetail accId dName nName ts) ->
  E.atKey' "accountId" accountIdEncoder accId .
  E.atKey' "displayName" E.text dName .
  maybeOrAbsentE "nickname" E.text nName .
  E.atKey' "transaction" transactionDetailsEncoder ts
