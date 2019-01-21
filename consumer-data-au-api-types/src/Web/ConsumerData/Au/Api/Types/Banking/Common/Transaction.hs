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

import           Control.Lens               (Prism', prism, ( # ))
import           Control.Monad.Except       (throwError)
import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text)
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types           (Json, MapLikeObj, WS)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
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
transactionsDecoder = D.atKey "transactions" (Transactions <$> D.list transactionDecoder)

transactionsEncoder :: Applicative f => Encoder f Transactions
transactionsEncoder = E.mapLikeObj $ E.atKey' "transactions" (E.list transactionEncoder) . unTransactions

instance JsonDecode OB Transactions where
  mkDecoder = tagOb transactionsDecoder

instance JsonEncode OB Transactions where
  mkEncoder = tagOb transactionsEncoder


data Transaction = Transaction
  { _transactionAccountId            :: AccountId
  , _transactionTransactionId        :: Maybe TransactionId
  , _transactionIsDetailAvailable    :: Bool
  , _transactionType                 :: TransactionType
  , _transactionStatus               :: TransactionStatus
  , _transactionDescription          :: Text
  , _transactionValueDateTime        :: Maybe DateTimeString
  , _transactionExecutionDateTime    :: Maybe DateTimeString
  , _transactionAmount               :: Maybe AmountString
  , _transactionCurrency             :: Maybe CurrencyString
  , _transactionReference            :: Text
  , _transactionMerchantName         :: Maybe Text
  , _transactionMerchantCategoryCode :: Maybe Text
  , _transactionBillerCode           :: Maybe Text
  , _transactionBillerName           :: Maybe Text
  , _transactionCrn                  :: Maybe Text
  , _transactionApcaNumber           :: Maybe Text
  } deriving (Eq, Show)

transactionDecoder :: (Monad f) => Decoder f Transaction
transactionDecoder =
  Transaction
    <$> D.atKey "accountId" accountIdDecoder
    <*> atKeyOptional' "transactionId" transactionIdDecoder
    <*> D.atKey "isDetailAvailable" D.bool
    <*> D.atKey "type" transactionTypeDecoder
    <*> transactionStatusDecoder
    <*> D.atKey "description" D.text
    <*> atKeyOptional' "valueDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "executionDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "amount" amountStringDecoder
    <*> atKeyOptional' "currency" currencyStringDecoder
    <*> D.atKey "reference" D.text
    <*> atKeyOptional' "merchantName" D.text
    <*> atKeyOptional' "merchantCategoryCode" D.text
    <*> atKeyOptional' "billerCode" D.text
    <*> atKeyOptional' "billerName" D.text
    <*> atKeyOptional' "crn" D.text
    <*> atKeyOptional' "apcaNumber" D.text

instance JsonDecode OB Transaction where
  mkDecoder = tagOb transactionDecoder

transactionEncoder :: Applicative f => Encoder f Transaction
transactionEncoder = E.mapLikeObj transactionMLO

transactionMLO :: Transaction -> MapLikeObj WS Json -> MapLikeObj WS Json
transactionMLO t =
  E.atKey' "accountId" accountIdEncoder (_transactionAccountId t) .
  maybeOrAbsentE "transactionId" transactionIdEncoder (_transactionTransactionId t) .
  E.atKey' "isDetailAvailable" E.bool (_transactionIsDetailAvailable t) .
  E.atKey' "type" transactionTypeEncoder (_transactionType t) .
  transactionStatusFields (_transactionStatus t) .
  E.atKey' "description" E.text (_transactionDescription t) .
  maybeOrAbsentE "valueDateTime" dateTimeStringEncoder (_transactionValueDateTime t) .
  maybeOrAbsentE "executionDateTime" dateTimeStringEncoder (_transactionExecutionDateTime t) .
  maybeOrAbsentE "amount" amountStringEncoder (_transactionAmount t) .
  maybeOrAbsentE "currency" currencyStringEncoder (_transactionCurrency t) .
  E.atKey' "reference" E.text (_transactionReference t) .
  maybeOrAbsentE "merchantName" E.text (_transactionMerchantName t) .
  maybeOrAbsentE "merchantCategoryCode" E.text (_transactionMerchantCategoryCode t) .
  maybeOrAbsentE "billerCode" E.text (_transactionBillerCode t) .
  maybeOrAbsentE "billerName" E.text (_transactionBillerName t) .
  maybeOrAbsentE "crn" E.text (_transactionCrn t) .
  maybeOrAbsentE "apcaNumber" E.text (_transactionApcaNumber t)

instance JsonEncode OB Transaction where
  mkEncoder = tagOb transactionEncoder


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


data TransactionType =
    TransactionTypeFee -- ^"FEE"
  | TransactionTypeInterestCharged -- ^"INTEREST_CHARGED"
  | TransactionTypeInterestPaid -- ^"INTEREST_PAID"
  | TransactionTypeTransferOutgoing -- ^"TRANSFER_OUTGOING"
  | TransactionTypeTransferIncoming -- ^"TRANSFER_INCOMING"
  | TransactionTypePayment -- ^"PAYMENT"
  | TransactionTypeOther -- ^"OTHER"
  deriving (Bounded, Enum, Eq, Ord, Show)

transactionTypeText ::
  Prism' Text TransactionType
transactionTypeText =
  prism (\case
        TransactionTypeFee -> "FEE"
        TransactionTypeInterestCharged -> "INTEREST_CHARGED"
        TransactionTypeInterestPaid -> "INTEREST_PAID"
        TransactionTypeTransferOutgoing -> "TRANSFER_OUTGOING"
        TransactionTypeTransferIncoming -> "TRANSFER_INCOMING"
        TransactionTypePayment -> "PAYMENT"
        TransactionTypeOther -> "OTHER"
    )
    (\case
        "FEE" -> Right TransactionTypeFee
        "INTEREST_CHARGED" -> Right TransactionTypeInterestCharged
        "INTEREST_PAID" -> Right TransactionTypeInterestPaid
        "TRANSFER_OUTGOING" -> Right TransactionTypeTransferOutgoing
        "TRANSFER_INCOMING" -> Right TransactionTypeTransferIncoming
        "PAYMENT" -> Right TransactionTypePayment
        "OTHER" -> Right TransactionTypeOther
        t -> Left t
    )

transactionTypeEncoder ::
  E.Encoder' TransactionType
transactionTypeEncoder =
  E.prismE transactionTypeText E.text'

transactionTypeDecoder :: Monad m =>
  D.Decoder m TransactionType
transactionTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid TransactionType")
  transactionTypeText
  D.text


data TransactionStatus =
    TransactionStatusPending -- ^ "PENDING"
  | TransactionStatusPosted DateTimeString -- ^ "POSTED"
  deriving (Eq, Show)

transactionStatusDecoder :: Monad f => Decoder f TransactionStatus
transactionStatusDecoder = do
  status <- D.atKey "status" D.text
  postingDateTime <- case status of
    "PENDING" -> pure TransactionStatusPending
    "POSTED" -> TransactionStatusPosted <$> (D.atKey "postingDateTime" dateTimeStringDecoder)
    _ -> throwError (D._ConversionFailure # "Not a valid TransactionStatus")
  pure postingDateTime

transactionStatus'ToText :: TransactionStatus' -> Text
transactionStatus'ToText = \case
  TransactionStatusPending' -> "PENDING"
  TransactionStatusPosted' -> "POSTED"

data TransactionStatus' =
    TransactionStatusPending'
  | TransactionStatusPosted'

transactionStatus'Encoder :: Applicative f => Encoder f TransactionStatus'
transactionStatus'Encoder = flip contramap E.text transactionStatus'ToText

transactionStatusToType' :: TransactionStatus -> TransactionStatus'
transactionStatusToType' (TransactionStatusPending {}) = TransactionStatusPending'
transactionStatusToType' (TransactionStatusPosted {}) = TransactionStatusPosted'

transactionStatusFields :: (Monoid ws, Semigroup ws) => TransactionStatus -> MapLikeObj ws Json -> MapLikeObj ws Json
transactionStatusFields ts =
  case ts of
    TransactionStatusPending ->
      E.atKey' "status" transactionStatus'Encoder (transactionStatusToType' ts)
    TransactionStatusPosted v ->
      E.atKey' "status" transactionStatus'Encoder (transactionStatusToType' ts) .
      E.atKey' "postingDateTime" dateTimeStringEncoder v
