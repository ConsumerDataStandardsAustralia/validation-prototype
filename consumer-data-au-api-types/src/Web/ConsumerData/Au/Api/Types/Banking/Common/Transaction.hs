{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
  ) where

import           Control.Lens               (Prism', prism, (#))
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types                    (Json, MapLikeObj, WS)

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, AsciiString, CurrencyString, DateTimeString,
    amountStringDecoder, amountStringEncoder, asciiStringDecoder,
    asciiStringEncoder, currencyStringDecoder, currencyStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)
import Web.ConsumerData.Au.Api.Types.Tag


newtype Transactions = Transactions
  { unTransactions :: [Transaction] }
  deriving (Eq, Show)

transactionsDecoder :: Monad f => Decoder f Transactions
transactionsDecoder =
  Transactions <$> D.list transactionDecoder

transactionsEncoder :: Applicative f => Encoder f Transactions
transactionsEncoder =
  unTransactions >$< E.list transactionEncoder

instance JsonDecode OB Transactions where
  mkDecoder = tagOb transactionsDecoder

instance JsonEncode OB Transactions where
  mkEncoder = tagOb transactionsEncoder


data Transaction = Transaction
  { _transactionTransactionId     :: Maybe TransactionId -- ^ A unique ID of the transaction adhering to the standards for ID permanence. This field is mandatory in this payload as it is a reflection of the requested transaction in the path parameter.
  , _transactionIsDetailAvailable :: Bool -- ^ True if extended information is available using the transaction detail end point. False if extended data is not available
  , _transactionStatus            :: TransactionStatus -- ^ Status of the transaction.
  , _transactionDescription       :: Text -- ^ The transaction description as applied by the financial institution.
  , _transactionPostDateTime      :: Maybe DateTimeString -- ^ The time the transaction was posted. This field is MANDATORY if the transaction has status POSTED. This is the time that appears on a standard statement.
  , _transactionExecutionDateTime :: Maybe DateTimeString -- ^ The time the transaction was executed by the originating customer, if available.
  , _transactionAmount            :: Maybe AmountString -- ^ The value of the transaction. Negative values mean money was outgoing.
  , _transactionCurrency          :: Maybe CurrencyString -- ^ The currency for the transaction amount. AUD assumed if not present.
  , _transactionReference         :: Text -- ^ The reference for the transaction provided by the originating institution.
  } deriving (Eq, Show)

transactionDecoder :: (Monad f) => Decoder f Transaction
transactionDecoder =
  Transaction
    <$> atKeyOptional' "transactionId" transactionIdDecoder
    <*> D.atKey "isDetailAvailable" D.bool
    <*> D.atKey "status" transactionStatusDecoder
    <*> D.atKey "description" D.text
    <*> atKeyOptional' "postDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "executionDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "amount" amountStringDecoder
    <*> atKeyOptional' "currency" currencyStringDecoder
    <*> D.atKey "reference" D.text

instance JsonDecode OB Transaction where
  mkDecoder = tagOb transactionDecoder

transactionEncoder :: Applicative f => Encoder f Transaction
transactionEncoder = E.mapLikeObj transactionMLO

transactionMLO :: Transaction -> MapLikeObj WS Json -> MapLikeObj WS Json
transactionMLO p =
  maybeOrAbsentE "transactionId" transactionIdEncoder (_transactionTransactionId p) .
  E.atKey' "isDetailAvailable" E.bool (_transactionIsDetailAvailable p) .
  E.atKey' "status" transactionStatusEncoder (_transactionStatus p) .
  E.atKey' "description" E.text (_transactionDescription p) .
  maybeOrAbsentE "postDateTime" dateTimeStringEncoder (_transactionPostDateTime p) .
  maybeOrAbsentE "executionDateTime" dateTimeStringEncoder (_transactionExecutionDateTime p) .
  maybeOrAbsentE "amount" amountStringEncoder (_transactionAmount p) .
  maybeOrAbsentE "currency" currencyStringEncoder (_transactionCurrency p) .
  E.atKey' "reference" E.text (_transactionReference p)

instance JsonEncode OB Transaction where
  mkEncoder = tagOb transactionEncoder


-- | A unique ID of the transaction adhering to the standards for ID permanence. This field is mandatory in this payload as it is a reflection of the requested transaction in the path parameter. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schematransactionid CDR AU v0.1.0 TransactionId>
newtype TransactionId =
  TransactionId { unTransactionId :: AsciiString }
  deriving (Eq, Show)

transactionIdDecoder :: Monad f => Decoder f TransactionId
transactionIdDecoder = TransactionId <$> asciiStringDecoder

transactionIdEncoder :: Applicative f => Encoder f TransactionId
transactionIdEncoder = unTransactionId >$< asciiStringEncoder

instance ToHttpApiData TransactionId where
  toUrlPiece = toUrlPiece . unTransactionId
instance FromHttpApiData TransactionId where
  parseUrlPiece = fmap TransactionId . parseUrlPiece


-- | Status of the transaction. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schematransactionstatus CDR AU v0.1.0 TransactionStatus>
data TransactionStatus =
    TransactionStatusPending -- ^ "PENDING"
  | TransactionStatusPosted -- ^ "POSTED"
  deriving (Bounded, Enum, Eq, Ord, Show)

transactionStatusText ::
  Prism' Text TransactionStatus
transactionStatusText =
  prism (\case
        TransactionStatusPending -> "PENDING"
        TransactionStatusPosted -> "POSTED"
    )
    (\case
        "PENDING" -> Right TransactionStatusPending
        "POSTED" -> Right TransactionStatusPosted
        t -> Left t
    )

transactionStatusEncoder ::
  E.Encoder' TransactionStatus
transactionStatusEncoder =
  E.prismE transactionStatusText E.text'

transactionStatusDecoder :: Monad m =>
  D.Decoder m TransactionStatus
transactionStatusDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid TransactionStatus")
  transactionStatusText
  D.text
