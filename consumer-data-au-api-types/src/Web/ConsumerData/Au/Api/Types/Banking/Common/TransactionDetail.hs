{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionDetail
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionDetail
  ) where

import           Data.Text         (Text)
import           GHC.Generics      (Generic)
import           Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import           Waargonaut.Encode (Encoder')
import qualified Waargonaut.Encode as E

import Web.ConsumerData.Au.Api.Types.Banking.Common.ExtendedTransactionData
    (ExtendedTransactionData, extendedTransactionDataDecoder,
    extendedTransactionDataEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionBasic
    (TransactionId, TransactionStatus, transactionIdDecoder,
    transactionIdEncoder, transactionStatusDecoder, transactionStatusEncoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, CurrencyString, DateTimeString, amountStringDecoder,
    amountStringEncoder, currencyStringDecoder, currencyStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)


-- | TransactionDetail <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schematransactiondetail CDR AU v0.1.0 TransactionDetail>
data TransactionDetail = TransactionDetail
  { _transactionDetailTransactionId     :: Maybe TransactionId -- ^  A unique ID of the transaction adhering to the standards for ID permanence. This field is mandatory in this payload as it is a reflection of the requested transaction in the path parameter.
  , _transactionDetailStatus            :: TransactionStatus -- ^ Status of the transaction.
  , _transactionDetailDescription       :: Text -- ^ The transaction description as applied by the financial institution.
  , _transactionDetailPostDateTime      :: Maybe DateTimeString -- ^ The time the transaction was posted. This field is MANDATORY if the transaction has status POSTED. This is the time that appears on a standard statement.
  , _transactionDetailExecutionDateTime :: Maybe DateTimeString -- ^ The time the transaction was executed by the originating customer, if available.
  , _transactionDetailAmount            :: Maybe AmountString -- ^ The value of the transaction. Negative values mean money was outgoing.
  , _transactionDetailCurrency          :: Maybe CurrencyString -- ^ The currency for the transaction amount. AUD assumed if not present.
  , _transactionDetailReference         :: Text -- ^ The reference for the transaction provided by the originating institution.
  , _transactionDetailExtendedData      :: Maybe ExtendedTransactionData -- ^ Contains more detailed information specific to transactions originated via NPP.
  } deriving (Generic, Show, Eq)

transactionDetailDecoder :: Monad f => Decoder f TransactionDetail
transactionDetailDecoder = D.withCursor $ \c -> do
  o <- D.down c
  tid <- D.fromKey "transactionId" (D.maybeOrNull transactionIdDecoder) o
  ts <- D.fromKey "transactionStatus" transactionStatusDecoder o
  desc <- D.fromKey "description" D.text o
  pdt <- D.fromKey "postDateTime" (D.maybeOrNull dateTimeStringDecoder) o
  edt <- D.fromKey "executionDateTime" (D.maybeOrNull dateTimeStringDecoder) o
  amt <- D.fromKey "amount" (D.maybeOrNull amountStringDecoder) o
  cur <- D.fromKey "currency" (D.maybeOrNull currencyStringDecoder) o
  ref <- D.fromKey "reference" D.text o
  ed <- D.fromKey "extendedData" (D.maybeOrNull extendedTransactionDataDecoder) o
  pure $ TransactionDetail tid ts desc pdt edt amt cur ref ed

transactionDetailEncoder :: Encoder' TransactionDetail
transactionDetailEncoder = E.mapLikeObj $ \(TransactionDetail tid ts desc pdt edt amt cur ref ed) ->
  E.atKey' "transactionId" (E.maybeOrNull transactionIdEncoder) tid .
  E.atKey' "transactionStatus" transactionStatusEncoder ts .
  E.atKey' "description" E.text desc .
  E.atKey' "postDateTime" (E.maybeOrNull dateTimeStringEncoder) pdt .
  E.atKey' "executionDateTime" (E.maybeOrNull dateTimeStringEncoder) edt .
  E.atKey' "amount" (E.maybeOrNull amountStringEncoder) amt .
  E.atKey' "currency" (E.maybeOrNull currencyStringEncoder) cur .
  E.atKey' "reference" E.text ref .
  E.atKey' "extendedData" (E.maybeOrNull extendedTransactionDataEncoder) ed
