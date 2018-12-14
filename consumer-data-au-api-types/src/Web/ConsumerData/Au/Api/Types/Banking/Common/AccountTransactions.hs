{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountTransactions
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountTransactions
  ) where

import           Data.Text         (Text)
import           Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import           Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
    (Transactions, transactionsDecoder, transactionsEncoder)
-- import Web.ConsumerData.Au.Api.Types.Tag                        (OB, tagOb)


data AccountTransactions = AccountTransactions
  { _transactionDetailsAccountId    :: AccountId
  , _transactionDetailsDisplayName  :: Text
  , _transactionDetailsNickname     :: Maybe Text
  , _transactionDetailsTransactions :: Transactions
  } deriving (Eq, Show)

accountTransactionDecoder :: Monad f => Decoder f AccountTransactions
accountTransactionDecoder =
  AccountTransactions
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey "transactions" transactionsDecoder

accountTransactionEncoder :: Applicative f => Encoder f AccountTransactions
accountTransactionEncoder = E.mapLikeObj $ \(AccountTransactions accId dName nName ts) ->
  E.atKey' "accountId" accountIdEncoder accId .
  E.atKey' "displayName" E.text dName .
  maybeOrAbsentE "nickname" E.text nName .
  E.atKey' "transactions" transactionsEncoder ts
