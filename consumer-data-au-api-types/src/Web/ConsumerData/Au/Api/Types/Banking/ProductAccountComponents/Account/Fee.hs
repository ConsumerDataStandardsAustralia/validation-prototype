{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Fee
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Fee
  ) where

import           Control.Monad.Except       (throwError)
import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text)
import           Text.URI                   (URI)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Discount
    (AccountDiscounts, accountDiscountsDecoder, accountDiscountsEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, CurrencyString, DurationString, RateString,
    amountStringDecoder, amountStringEncoder, currencyStringDecoder,
    currencyStringEncoder, durationStringDecoder, durationStringEncoder,
    rateStringDecoder, rateStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype AccountFees =
  AccountFees { getAccountFees :: [AccountFee] }
  deriving (Eq, Show)

accountFeesDecoder :: Monad f => Decoder f AccountFees
accountFeesDecoder = AccountFees <$> D.list accountFeeDecoder

accountFeesEncoder :: Applicative f => Encoder f AccountFees
accountFeesEncoder = getAccountFees >$< E.list accountFeeEncoder

instance JsonDecode OB AccountFees where
  mkDecoder = tagOb accountFeesDecoder

instance JsonEncode OB AccountFees where
  mkEncoder = tagOb accountFeesEncoder



-- | ProductFee <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductfee CDR AU v0.1.0 ProductFee>
-- WARNING This type is refering to *Account* only property, not as it defined in the above link
data AccountFee = AccountFee
  { _accountFeeName              :: Text -- ^ Name of the fee
  , _accountFeeFeeType           :: AccountFeeType -- ^ The type of fee. See the note below for valid values and their meaning
-- WARNING
  , _accountFeeAmount            :: Maybe AmountString -- ^ The amount charged for the fee. Assumed to be in AUD. One of amount, balanceRate and transactionRate is mandatory.
  , _accountFeeBalanceRate       :: Maybe RateString -- ^ A fee rate calculated based on a proportion of the balance. Assumed to be in AUD. One of amount, balanceRate and transactionRate is mandatory.
  , _accountFeeTransactionRate   :: Maybe RateString -- ^ A fee rate calculated based on a proportion of a transaction. Assumed to be in AUD. One of amount, balanceRate and transactionRate is mandatory.
  , _accountFeeCurrency          :: Maybe CurrencyString -- ^ The currency the fee will be charged in. Assumes AUD if absent
  , _accountFeeAdditionalInfo    :: Maybe Text -- ^ Display text providing more information on the fee
  , _accountFeeAdditionalInfoUri :: Maybe URI -- ^ Optional Link to a web page with more information on this fee
  , _accountFeeDiscounts         :: Maybe AccountDiscounts -- ^ An optional list of discounts to this fee that may be available
-- WARNING
  } deriving (Show, Eq)

accountFeeDecoder :: Monad f => Decoder f AccountFee
accountFeeDecoder =
  AccountFee
    <$> D.atKey "name" D.text
    <*> accountFeeTypeDecoder
    <*> atKeyOptional' "amount" amountStringDecoder
    <*> atKeyOptional' "balanceRate" rateStringDecoder
    <*> atKeyOptional' "transactionRate" rateStringDecoder
    <*> atKeyOptional' "currency" currencyStringDecoder
    <*> atKeyOptional' "additionalInfo" D.text
    <*> atKeyOptional' "additionalInfoUri" uriDecoder
    <*> atKeyOptional' "discounts" accountDiscountsDecoder

instance JsonDecode OB AccountFee where
  mkDecoder = tagOb accountFeeDecoder

accountFeeEncoder :: Applicative f => Encoder f AccountFee
accountFeeEncoder = E.mapLikeObj $ \p ->
  E.atKey' "name" E.text (_accountFeeName p) .
  accountFeeTypeFields (_accountFeeFeeType p) .
  maybeOrAbsentE "amount" amountStringEncoder (_accountFeeAmount p) .
  maybeOrAbsentE "balanceRate" rateStringEncoder (_accountFeeBalanceRate p) .
  maybeOrAbsentE "transactionRate" rateStringEncoder (_accountFeeTransactionRate p) .
  maybeOrAbsentE "currency" currencyStringEncoder (_accountFeeCurrency p) .
  maybeOrAbsentE "additionalInfo" E.text (_accountFeeAdditionalInfo p) .
  maybeOrAbsentE "additionalInfoUri" uriEncoder (_accountFeeAdditionalInfoUri p) .
  maybeOrAbsentE "discounts" accountDiscountsEncoder (_accountFeeDiscounts p)

instance JsonEncode OB AccountFee where
  mkEncoder = tagOb accountFeeEncoder


-- | Description of the usage of the @feeType@ field as it applies to accounts. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#accountfeetypedoc CDR AU v0.1.0 Account Fee Types>
data AccountFeeType =
    AFeePeriodicPeriodic DurationString -- ^ "PERIODIC" A periodic fee such as a monthly account servicing fee. Use of @additionalValue@ field: The period of charge. Formatted according to ISO 8601 Durations.
  | AFeePeriodicTransaction Text -- ^ "TRANSACTION" A fee for each transaction (above any free transactions in a period). Use of @additionalValue@ field: A description of the type of transaction (eg. Assisted Transaction, Teller Transaction, Cheque).
-- not for accounts
-- | AFeePeriodicEstablishment -- ^ "ESTABLISHMENT" An establishment fee for the product.
  | AFeePeriodicExit -- ^ "EXIT" A fee for closing the product.
  | AFeePeriodicOverdraw -- ^ "OVERDRAW" A fee for overdrawing the account.
  | AFeePeriodicMinBalance DurationString -- ^ "MIN_BALANCE" A periodic fee for being below the minimum balance. Use of @additionalValue@ field: The period of charge. Formatted according to ISO 8601 Durations.
  | AFeePeriodicRedraw -- ^ "REDRAW" A fee for performing a redraw transaction.
  | AFeePeriodicChequeCash -- ^ "CHEQUE_CASH" A fee for cashing a cheque.
  | AFeePeriodicChequeStop -- ^ "CHEQUE_STOP" A fee for stopping a cheque.
  | AFeePeriodicChequeBook -- ^ "CHEQUE_BOOK" A fee for ordering a new cheque book.
  | AFeePeriodicCardReplace -- ^ "CARD_REPLACE" A fee for ordering a replacement card.
  | AFeePeriodicPaperStatement -- ^ "PAPER_STATEMENT" A fee for obtaining a paper statement.
  | AFeePeriodicOtherEvent Text -- ^ "OTHER_EVENT" A fee for another type of event not already specified in the list of valid values. Use of @additionalValue@ field: Text description of the event.
  deriving (Show, Eq)

accountFeeTypeDecoder :: Monad f => Decoder f AccountFeeType
accountFeeTypeDecoder = do
  feeType <- D.atKey "feeType" D.text
  additionalValue <- case feeType of
    "PERIODIC" -> AFeePeriodicPeriodic <$> (additionalValueDecoder durationStringDecoder)
    "TRANSACTION" -> AFeePeriodicTransaction <$> (additionalValueDecoder D.text)
    "EXIT" -> pure AFeePeriodicExit
    "OVERDRAW" -> pure AFeePeriodicOverdraw
    "MIN_BALANCE" -> AFeePeriodicMinBalance <$> (additionalValueDecoder durationStringDecoder)
    "REDRAW" -> pure AFeePeriodicRedraw
    "CHEQUE_CASH" -> pure AFeePeriodicChequeCash
    "CHEQUE_STOP" -> pure AFeePeriodicChequeStop
    "CHEQUE_BOOK" -> pure AFeePeriodicChequeBook
    "CARD_REPLACE" -> pure AFeePeriodicCardReplace
    "PAPER_STATEMENT" -> pure AFeePeriodicPaperStatement
    "OTHER_EVENT" -> AFeePeriodicOtherEvent <$> (additionalValueDecoder D.text)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

accountFeeType'ToText :: AccountFeeType' -> Text
accountFeeType'ToText = \case
  AFeePeriodicPeriodic' -> "PERIODIC"
  AFeePeriodicTransaction' -> "TRANSACTION"
  AFeePeriodicExit' -> "EXIT"
  AFeePeriodicOverdraw' -> "OVERDRAW"
  AFeePeriodicMinBalance' -> "MIN_BALANCE"
  AFeePeriodicRedraw' -> "REDRAW"
  AFeePeriodicChequeCash' -> "CHEQUE_CASH"
  AFeePeriodicChequeStop' -> "CHEQUE_STOP"
  AFeePeriodicChequeBook' -> "CHEQUE_BOOK"
  AFeePeriodicCardReplace' -> "CARD_REPLACE"
  AFeePeriodicPaperStatement' -> "PAPER_STATEMENT"
  AFeePeriodicOtherEvent' -> "OTHER_EVENT"

data AccountFeeType' =
    AFeePeriodicPeriodic'
  | AFeePeriodicTransaction'
  | AFeePeriodicExit'
  | AFeePeriodicOverdraw'
  | AFeePeriodicMinBalance'
  | AFeePeriodicRedraw'
  | AFeePeriodicChequeCash'
  | AFeePeriodicChequeStop'
  | AFeePeriodicChequeBook'
  | AFeePeriodicCardReplace'
  | AFeePeriodicPaperStatement'
  | AFeePeriodicOtherEvent'
  deriving (Eq, Show)

accountFeeType'Encoder :: Applicative f => Encoder f AccountFeeType'
accountFeeType'Encoder = flip contramap E.text accountFeeType'ToText

accountFeeTypeToType' :: AccountFeeType -> AccountFeeType'
accountFeeTypeToType' (AFeePeriodicPeriodic {}) = AFeePeriodicPeriodic'
accountFeeTypeToType' (AFeePeriodicTransaction {}) = AFeePeriodicTransaction'
accountFeeTypeToType' (AFeePeriodicExit {}) = AFeePeriodicExit'
accountFeeTypeToType' (AFeePeriodicOverdraw {}) = AFeePeriodicOverdraw'
accountFeeTypeToType' (AFeePeriodicMinBalance {}) = AFeePeriodicMinBalance'
accountFeeTypeToType' (AFeePeriodicRedraw {}) = AFeePeriodicRedraw'
accountFeeTypeToType' (AFeePeriodicChequeCash {}) = AFeePeriodicChequeCash'
accountFeeTypeToType' (AFeePeriodicChequeStop {}) = AFeePeriodicChequeStop'
accountFeeTypeToType' (AFeePeriodicChequeBook {}) = AFeePeriodicChequeBook'
accountFeeTypeToType' (AFeePeriodicCardReplace {}) = AFeePeriodicCardReplace'
accountFeeTypeToType' (AFeePeriodicPaperStatement {}) = AFeePeriodicPaperStatement'
accountFeeTypeToType' (AFeePeriodicOtherEvent {}) = AFeePeriodicOtherEvent'

accountFeeTypeFields :: (Monoid ws, Semigroup ws) => AccountFeeType -> MapLikeObj ws Json -> MapLikeObj ws Json
accountFeeTypeFields pc =
  case pc of
    AFeePeriodicPeriodic v ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    AFeePeriodicTransaction v ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    AFeePeriodicExit ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicOverdraw ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicMinBalance v ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    AFeePeriodicRedraw ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicChequeCash ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicChequeStop ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicChequeBook ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicCardReplace ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicPaperStatement ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc)
    AFeePeriodicOtherEvent v ->
      E.atKey' "feeType" accountFeeType'Encoder (accountFeeTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB AccountFeeType where
  mkDecoder = tagOb accountFeeTypeDecoder
