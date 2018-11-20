{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountTransaction
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountTransaction
  ) where

import           Data.Functor.Contravariant          ((>$<))
import           GHC.Generics                        (Generic)
import           Waargonaut.Decode                   (Decoder)
import qualified Waargonaut.Decode                   as D
import           Waargonaut.Encode                   (Encoder)
import qualified Waargonaut.Encode                   as E
import           Waargonaut.Generic                  (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Identified (Identified, identifiedDecoder, identifiedEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionBasic (TransactionBasic, transactionBasicDecoder, transactionBasicEncoder, transactionBasicMLO)
import Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionDetail (TransactionDetail, transactionDetailDecoder, transactionDetailEncoder)
import Web.ConsumerData.Au.Api.Types.Tag (OB, tagOb)

-- | AccountTransaction <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaaccounttransaction CDR AU v0.1.0 AccountTransaction>
data AccountTransaction = AccountTransaction
  { _transactionDetailAccountId :: AccountId -- ^ A unique ID of the account adhering to the standards for ID permanence.
  , _transactionDetailTransactionBasic :: Maybe TransactionBasic
  } deriving (Generic, Show, Eq)

accountTransactionDecoder :: Monad f => Decoder f AccountTransaction
accountTransactionDecoder = D.withCursor $ \c -> do
  o <- D.down c
  accId <- D.fromKey "accountId" accountIdDecoder o
  basic <- D.try $ D.focus transactionBasicDecoder c
  pure $ AccountTransaction accId basic

accountTransactionEncoder :: Applicative f => Encoder f AccountTransaction
accountTransactionEncoder = E.mapLikeObj $ \(AccountTransaction accId tb) ->
  E.atKey' "accountId" accountIdEncoder accId .
  maybe id transactionBasicMLO tb

-- | <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaaccounttransactionsresponse>
newtype AccountTransactions = AccountTransactions
  { unAccountTransactions :: Identified [TransactionBasic] }
  deriving (Generic, Show, Eq)

accountTransactionsDecoder :: Monad f => Decoder f AccountTransactions
accountTransactionsDecoder =
  AccountTransactions <$> identifiedDecoder "transactions" (D.list transactionBasicDecoder)

accountTransactionsEncoder :: Applicative f => Encoder f AccountTransactions
accountTransactionsEncoder = unAccountTransactions >$< identifiedEncoder "transactions" (E.list transactionBasicEncoder)

instance JsonDecode OB AccountTransactions where
  mkDecoder = tagOb accountTransactionsDecoder

instance JsonEncode OB AccountTransactions where
  mkEncoder = tagOb accountTransactionsEncoder

newtype AccountsTransactions = AccountsTransactions
  { _accountsTransactionsTransactions :: [AccountTransaction]
  } deriving (Generic, Show, Eq)

instance JsonDecode OB AccountsTransactions where
  mkDecoder = tagOb accountsTransactionsDecoder

instance JsonEncode OB AccountsTransactions where
  mkEncoder = tagOb accountsTransactionsEncoder

accountsTransactionsDecoder :: Monad f => Decoder f AccountsTransactions
accountsTransactionsDecoder = D.withCursor $ \c -> do
  o <- D.down c
  ts <- D.fromKey "transactions" (D.list accountTransactionDecoder) o
  pure $ AccountsTransactions ts

accountsTransactionsEncoder :: Applicative f => Encoder f AccountsTransactions
accountsTransactionsEncoder = E.mapLikeObj $ \(AccountsTransactions ts) ->
  E.atKey' "transactions" (E.list accountTransactionEncoder) ts

-- | <https://consumerdatastandardsaustralia.github.io/standards/?swagger#get-bulk-transactions>
newtype AccountTransactionDetail = AccountTransactionDetail
  { unAccountTransactionDetail :: Identified TransactionDetail }

accountTransactionDetailDecoder :: Monad f => Decoder f AccountTransactionDetail
accountTransactionDetailDecoder =
  AccountTransactionDetail <$> identifiedDecoder "transaction" transactionDetailDecoder

accountTransactionDetailEncoder :: Applicative f => Encoder f AccountTransactionDetail
accountTransactionDetailEncoder = unAccountTransactionDetail >$< identifiedEncoder "transaction" transactionDetailEncoder

instance JsonDecode OB AccountTransactionDetail where
  mkDecoder = tagOb accountTransactionDetailDecoder

instance JsonEncode OB AccountTransactionDetail where
  mkEncoder = tagOb accountTransactionDetailEncoder
