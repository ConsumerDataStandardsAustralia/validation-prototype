{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.BulkTransaction
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.BulkTransaction
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E
import           Waargonaut.Generic      (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types        (Json, MapLikeObj, WS)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
    (TransactionId, transactionIdDecoder, transactionIdEncoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, CurrencyString, DateTimeString, amountStringDecoder,
    amountStringEncoder, currencyStringDecoder, currencyStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)
import Web.ConsumerData.Au.Api.Types.Tag


newtype BulkTransactions = BulkTransactions { unBulkTransactions :: [BulkTransaction] } deriving (Eq, Show)

bulkTransactionsDecoder :: Monad f => Decoder f BulkTransactions
bulkTransactionsDecoder = D.atKey "transactions" (BulkTransactions <$> D.list bulkTransactionDecoder)

bulkTransactionsEncoder :: Applicative f => Encoder f BulkTransactions
bulkTransactionsEncoder = E.mapLikeObj $ E.atKey' "transactions" (E.list bulkTransactionEncoder) . unBulkTransactions

instance JsonDecode OB BulkTransactions where
  mkDecoder = tagOb bulkTransactionsDecoder

instance JsonEncode OB BulkTransactions where
  mkEncoder = tagOb bulkTransactionsEncoder


data BulkTransaction = BulkTransaction
  { _bulkTransactionAccountId         :: AccountId
  , _bulkTransactionTransactionId     :: Maybe TransactionId -- ^ A unique ID of the bulkTransaction adhering to the standards for ID permanence. This field is mandatory in this payload as it is a reflection of the requested bulkTransaction in the path parameter.
  , _bulkTransactionIsDetailAvailable :: Bool -- ^ True if extended information is available using the bulkTransaction detail end point. False if extended data is not available
  , _bulkTransactionStatus            :: BulkTransactionStatus -- ^ Status of the bulkTransaction.
  , _bulkTransactionDescription       :: Text -- ^ The bulkTransaction description as applied by the financial institution.
  , _bulkTransactionPostDateTime      :: Maybe DateTimeString -- ^ The time the bulkTransaction was posted. This field is MANDATORY if the bulkTransaction has status POSTED. This is the time that appears on a standard statement.
  , _bulkTransactionExecutionDateTime :: Maybe DateTimeString -- ^ The time the bulkTransaction was executed by the originating customer, if available.
  , _bulkTransactionAmount            :: Maybe AmountString -- ^ The value of the bulkTransaction. Negative values mean money was outgoing.
  , _bulkTransactionCurrency          :: Maybe CurrencyString -- ^ The currency for the bulkTransaction amount. AUD assumed if not present.
  , _bulkTransactionReference         :: Text -- ^ The reference for the bulkTransaction provided by the originating institution.
  } deriving (Eq, Show)

bulkTransactionDecoder :: (Monad f) => Decoder f BulkTransaction
bulkTransactionDecoder =
  BulkTransaction
    <$> D.atKey "accountId" accountIdDecoder
    <*> atKeyOptional' "transactionId" transactionIdDecoder
    <*> D.atKey "isDetailAvailable" D.bool
    <*> D.atKey "status" bulkTransactionStatusDecoder
    <*> D.atKey "description" D.text
    <*> atKeyOptional' "postDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "executionDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "amount" amountStringDecoder
    <*> atKeyOptional' "currency" currencyStringDecoder
    <*> D.atKey "reference" D.text

bulkTransactionEncoder :: Applicative f => Encoder f BulkTransaction
bulkTransactionEncoder = E.mapLikeObj bulkTransactionMLO

bulkTransactionMLO :: BulkTransaction -> MapLikeObj WS Json -> MapLikeObj WS Json
bulkTransactionMLO t =
  E.atKey' "accountId" accountIdEncoder (_bulkTransactionAccountId t) .
  maybeOrAbsentE "transactionId" transactionIdEncoder (_bulkTransactionTransactionId t) .
  E.atKey' "isDetailAvailable" E.bool (_bulkTransactionIsDetailAvailable t) .
  E.atKey' "status" bulkTransactionStatusEncoder (_bulkTransactionStatus t) .
  E.atKey' "description" E.text (_bulkTransactionDescription t) .
  maybeOrAbsentE "postDateTime" dateTimeStringEncoder (_bulkTransactionPostDateTime t) .
  maybeOrAbsentE "executionDateTime" dateTimeStringEncoder (_bulkTransactionExecutionDateTime t) .
  maybeOrAbsentE "amount" amountStringEncoder (_bulkTransactionAmount t) .
  maybeOrAbsentE "currency" currencyStringEncoder (_bulkTransactionCurrency t) .
  E.atKey' "reference" E.text (_bulkTransactionReference t)


-- | Status of the bulkTransaction. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemabulkTransactionstatus CDR AU v0.1.0 BulkTransactionStatus>
data BulkTransactionStatus =
    BulkTransactionStatusPending -- ^ "PENDING"
  | BulkTransactionStatusPosted -- ^ "POSTED"
  deriving (Bounded, Enum, Eq, Ord, Show)

bulkTransactionStatusText ::
  Prism' Text BulkTransactionStatus
bulkTransactionStatusText =
  prism (\case
        BulkTransactionStatusPending -> "PENDING"
        BulkTransactionStatusPosted -> "POSTED"
    )
    (\case
        "PENDING" -> Right BulkTransactionStatusPending
        "POSTED" -> Right BulkTransactionStatusPosted
        t -> Left t
    )

bulkTransactionStatusEncoder ::
  E.Encoder' BulkTransactionStatus
bulkTransactionStatusEncoder =
  E.prismE bulkTransactionStatusText E.text'

bulkTransactionStatusDecoder :: Monad m =>
  D.Decoder m BulkTransactionStatus
bulkTransactionStatusDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid BulkTransactionStatus")
  bulkTransactionStatusText
  D.text
