{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionBasic
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionBasic
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


-- | TransactionBasic <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schematransactionbasic CDR AU v0.1.0 TransactionBasic>
data TransactionBasic = TransactionBasic
  { _transactionBasicTransactionId     :: Maybe TransactionId -- ^ A unique ID of the transaction adhering to the standards for ID permanence. This field is mandatory in this payload as it is a reflection of the requested transaction in the path parameter.
  , _transactionBasicIsDetailAvailable :: Bool -- ^ True if extended information is available using the transaction detail end point. False if extended data is not available
  , _transactionBasicStatus            :: TransactionStatus -- ^ Status of the transaction.
  , _transactionBasicDescription       :: Text -- ^ The transaction description as applied by the financial institution.
  , _transactionBasicPostDateTime      :: Maybe DateTimeString -- ^ The time the transaction was posted. This field is MANDATORY if the transaction has status POSTED. This is the time that appears on a standard statement.
  , _transactionBasicExecutionDateTime :: Maybe DateTimeString -- ^ The time the transaction was executed by the originating customer, if available.
  , _transactionBasicAmount            :: Maybe AmountString -- ^ The value of the transaction. Negative values mean money was outgoing.
  , _transactionBasicCurrency          :: Maybe CurrencyString -- ^ The currency for the transaction amount. AUD assumed if not present.
  , _transactionBasicReference         :: Text -- ^ The reference for the transaction provided by the originating institution.
  } deriving (Eq, Show)

transactionBasicDecoder :: (Monad f) => Decoder f TransactionBasic
transactionBasicDecoder =
  TransactionBasic
    <$> atKeyOptional' "transactionId" transactionIdDecoder
    <*> D.atKey "isDetailAvailable" D.bool
    <*> D.atKey "status" transactionStatusDecoder
    <*> D.atKey "description" D.text
    <*> atKeyOptional' "postDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "executionDateTime" dateTimeStringDecoder
    <*> atKeyOptional' "amount" amountStringDecoder
    <*> atKeyOptional' "currency" currencyStringDecoder
    <*> D.atKey "reference" D.text

instance JsonDecode OB TransactionBasic where
  mkDecoder = tagOb transactionBasicDecoder

transactionBasicEncoder :: Applicative f => Encoder f TransactionBasic
transactionBasicEncoder = E.mapLikeObj transactionBasicMLO

transactionBasicMLO :: TransactionBasic -> MapLikeObj WS Json -> MapLikeObj WS Json
transactionBasicMLO p =
  maybeOrAbsentE "transactionId" transactionIdEncoder (_transactionBasicTransactionId p) .
  E.atKey' "isDetailAvailable" E.bool (_transactionBasicIsDetailAvailable p) .
  E.atKey' "status" transactionStatusEncoder (_transactionBasicStatus p) .
  E.atKey' "description" E.text (_transactionBasicDescription p) .
  maybeOrAbsentE "postDateTime" dateTimeStringEncoder (_transactionBasicPostDateTime p) .
  maybeOrAbsentE "executionDateTime" dateTimeStringEncoder (_transactionBasicExecutionDateTime p) .
  maybeOrAbsentE "amount" amountStringEncoder (_transactionBasicAmount p) .
  maybeOrAbsentE "currency" currencyStringEncoder (_transactionBasicCurrency p) .
  E.atKey' "reference" E.text (_transactionBasicReference p)

instance JsonEncode OB TransactionBasic where
  mkEncoder = tagOb transactionBasicEncoder


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
  deriving (Show, Eq)

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
