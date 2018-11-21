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

import           Control.Lens               (Prism', prism, (^?))
import           Control.Monad.Error.Lens   (throwing)
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
transactionBasicDecoder = D.withCursor $ \c -> do
  o <- D.down c
  transactId <- D.fromKey "transactionId" (D.maybeOrNull transactionIdDecoder) o
  isDetailAval <- D.fromKey "isDetailAvailable" D.bool o
  status <- D.fromKey "status" transactionStatusDecoder o
  descr <- D.fromKey "description" D.text o
  postDateTime <- D.fromKey "postDateTime" (D.maybeOrNull dateTimeStringDecoder) o
  execDateTime <- D.fromKey "executionDateTime" (D.maybeOrNull dateTimeStringDecoder) o
  amount <- D.fromKey "amount" (D.maybeOrNull amountStringDecoder) o
  currency <- D.fromKey "currency" (D.maybeOrNull currencyStringDecoder) o
  reference <- D.fromKey "reference" D.text o
  pure $ TransactionBasic transactId isDetailAval status descr postDateTime execDateTime amount currency reference

instance JsonDecode OB TransactionBasic where
  mkDecoder = tagOb transactionBasicDecoder

transactionBasicEncoder :: Applicative f => Encoder f TransactionBasic
transactionBasicEncoder = E.mapLikeObj transactionBasicMLO

transactionBasicMLO :: TransactionBasic -> MapLikeObj WS Json -> MapLikeObj WS Json
transactionBasicMLO p =
  E.atKey' "transactionId" (E.maybeOrNull transactionIdEncoder) (_transactionBasicTransactionId p) .
  E.atKey' "isDetailAvailable" E.bool (_transactionBasicIsDetailAvailable p) .
  E.atKey' "status" transactionStatusEncoder (_transactionBasicStatus p) .
  E.atKey' "description" E.text (_transactionBasicDescription p) .
  E.atKey' "postDateTime" (E.maybeOrNull dateTimeStringEncoder) (_transactionBasicPostDateTime p) .
  E.atKey' "executionDateTime" (E.maybeOrNull dateTimeStringEncoder) (_transactionBasicExecutionDateTime p) .
  E.atKey' "amount" (E.maybeOrNull amountStringEncoder) (_transactionBasicAmount p) .
  E.atKey' "currency" (E.maybeOrNull currencyStringEncoder) (_transactionBasicCurrency p) .
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

--transactionStatusDecoder :: (MonadError D.DecodeError m, Monad m) =>
transactionStatusDecoder :: Monad m =>
  D.Decoder m TransactionStatus
transactionStatusDecoder = do
  tsMay <- (^? transactionStatusText) <$> D.text
  D.withCursor . const $ maybe
    (throwing D._ConversionFailure $ "is not a valid Transaction Status")
    pure
    tsMay
  -- Replace it with this later once Decoder gets a monadfail.
  --D.prismDOrFail
  --(D._ConversionFailure # "Not a valid TransactionStatus")
  --transactionStatusText
  --D.text
