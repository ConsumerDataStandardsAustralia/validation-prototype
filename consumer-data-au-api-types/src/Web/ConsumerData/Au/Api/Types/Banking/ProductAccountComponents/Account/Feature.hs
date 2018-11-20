{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Feature
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Feature
  ) where

import           Control.Monad.Except       (throwError)
import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, DurationString, amountStringDecoder, amountStringEncoder,
    durationStringDecoder, durationStringEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype AccountFeatures =
  AccountFeatures { getAccountFeatures :: [AccountFeatureType] }
  deriving (Eq, Show)

accountFeaturesDecoder :: Monad f => Decoder f AccountFeatures
accountFeaturesDecoder = AccountFeatures <$> D.list accountFeatureTypeDecoder

accountFeaturesEncoder :: Applicative f => Encoder f AccountFeatures
accountFeaturesEncoder = getAccountFeatures >$< E.list accountFeatureTypeEncoder

instance JsonDecode OB AccountFeatures where
  mkDecoder = tagOb accountFeaturesDecoder

instance JsonEncode OB AccountFeatures where
  mkEncoder = tagOb accountFeaturesEncoder


-- | AccountFeature = AccountFeatureType <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductfeature CDR AU v0.1.0 ProductFeature>
-- WARNING This type is refering to *Account* only property, not as it defined in the above link
-- | Description of the usage of the @featureType@ field as it applies to accounts. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#accountfeaturetypedoc CDR AU v0.1.0 Account Feature Types>
data AccountFeatureType =
    AFeatureCardAcess -- ^ "CARD_ACCESS" A card is available for the product to access funds.
  | AFeatureAdditionalCards Int -- ^ "ADDITIONAL_CARDS" Additional cards can be requested. Use of @additionalValue@ field: The maximum number of additional cards. If no maximum then should be set to null.
  | AFeatureUnlimitedTxns -- ^ "UNLIMITED_TXNS" Unlimited free transactions available.
  | AFeatureFreeTxns Int -- ^ "FREE_TXNS" A set number of free transactions available per month. Use of @additionalValue@ field: The number of free transactions.
  | AFeatureFreeTxnsAllowance AmountString -- ^ "FREE_TXNS_ALLOWANCE" A set amount of transaction fee value that is discounted per month. Use of @additionalValue@ field: The amount of transaction fee discounted (in AUD).
  | AFeatureLoyaltyProgram Text -- ^ "LOYALTY_PROGRAM" A points based loyalty program is available. Use of @additionalValue@ field: Name of the loyalty program.
  | AFeatureOffset-- ^ "OFFSET" An offset account can be connected to the product.
  | AFeatureOverdraft -- ^ "OVERDRAFT" An overdraft can be applied for.
  | AFeatureRedraw -- ^ "REDRAW" Redraw of repaid principal above minimum required is available.
  | AFeatureInsurance Text -- ^ "INSURANCE" Insurance is provided as an additional feature of the product. Use of @additionalValue@ field: Text description of the type of insurance (e.g. Travel Insurance).
  | AFeatureBalanceTransfers -- ^ "BALANCE_TRANSFERS" Balance transfers can be made from the account (eg. for credit cards).
  | AFeatureInterestFree DurationString -- ^ "INTEREST_FREE" Interest free period for purchases. Use of @additionalValue@ field:      Interest free period. Formatted according to ISO 8601 Durations.
  | AFeatureInterestFreeTransfers DurationString -- ^ "INTEREST_FREE_TRANSFERS" Interest free period for balance transfers. Use of @additionalValue@ field:      Interest free period. Formatted according to ISO 8601 Durations.
  | AFeatureDigitalWallet Text -- ^ "DIGITAL_WALLET" A Digital wallet can be attached to the product. Use of @additionalValue@ field: The name or brand of the wallet.
  | AFeatureDigitalBanking -- ^ "DIGITAL_BANKING" Access is available to online banking features for the product.
  | AFeatureNppPayid-- ^ "NPP_PAYID" An account of this product type can be used as the target of an NPP PayID.
  | AFeatureNppEnabled -- ^ "NPP_ENABLED" An account of this product type can be used to receive funds as a result of a BSB/Number based NPP payment.
  | AFeatureDonateInterest -- ^ "DONATE_INTEREST" Indicates that interest generated from the product can be automatically donated to a charity or community group.
  | AFeatureBillPayment Text -- ^ "BILL_PAYMENT" The product can be attached to an automatic budgeting and bill payment service. Use of @additionalValue@ field: Optional name of the service.
  deriving (Show, Eq)

accountFeatureTypeDecoder :: Monad f => Decoder f AccountFeatureType
accountFeatureTypeDecoder = D.withCursor $ \c -> do
  -- D.focus D.text c >>= \case
  o <- D.down c
  featureType <- D.fromKey "featureType" D.text o
  additionalValue <- case featureType of
    "CARD_ACCESS" -> pure AFeatureCardAcess
    "ADDITIONAL_CARDS" -> AFeatureAdditionalCards <$> (additionalValueDecoder D.int o)
    "UNLIMITED_TXNS" -> pure AFeatureUnlimitedTxns
    "FREE_TXNS" -> AFeatureFreeTxns <$> (additionalValueDecoder D.int o)
    "FREE_TXNS_ALLOWANCE" -> AFeatureFreeTxnsAllowance <$> (additionalValueDecoder amountStringDecoder o)
    "LOYALTY_PROGRAM" -> AFeatureLoyaltyProgram <$> (additionalValueDecoder D.text o)
    "OFFSET" -> pure AFeatureOffset
    "OVERDRAFT" -> pure AFeatureOverdraft
    "REDRAW" -> pure AFeatureRedraw
    "INSURANCE" -> AFeatureInsurance <$> (additionalValueDecoder D.text o)
    "BALANCE_TRANSFERS" -> pure AFeatureBalanceTransfers
    "INTEREST_FREE" -> AFeatureInterestFree <$> (additionalValueDecoder durationStringDecoder o)
    "INTEREST_FREE_TRANSFERS" -> AFeatureInterestFreeTransfers <$> (additionalValueDecoder durationStringDecoder o)
    "DIGITAL_WALLET" -> AFeatureDigitalWallet <$> (additionalValueDecoder D.text o)
    "DIGITAL_BANKING" -> pure AFeatureDigitalBanking
    "NPP_PAYID" -> pure AFeatureNppPayid
    "NPP_ENABLED" -> pure AFeatureNppEnabled
    "DONATE_INTEREST" -> pure AFeatureDonateInterest
    "BILL_PAYMENT" -> AFeatureBillPayment <$> (additionalValueDecoder D.text o)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

accountFeatureType'ToText :: AccountFeatureType' -> Text
accountFeatureType'ToText = \case
  AFeatureCardAcess' -> "CARD_ACCESS"
  AFeatureAdditionalCards' -> "ADDITIONAL_CARDS"
  AFeatureUnlimitedTxns' -> "UNLIMITED_TXNS"
  AFeatureFreeTxns' -> "FREE_TXNS"
  AFeatureFreeTxnsAllowance' -> "FREE_TXNS_ALLOWANCE"
  AFeatureLoyaltyProgram' -> "LOYALTY_PROGRAM"
  AFeatureOffset' -> "OFFSET"
  AFeatureOverdraft' -> "OVERDRAFT"
  AFeatureRedraw' -> "REDRAW"
  AFeatureInsurance' -> "INSURANCE"
  AFeatureBalanceTransfers' -> "BALANCE_TRANSFERS"
  AFeatureInterestFree' -> "INTEREST_FREE"
  AFeatureInterestFreeTransfers' -> "INTEREST_FREE_TRANSFERS"
  AFeatureDigitalWallet' -> "DIGITAL_WALLET"
  AFeatureDigitalBanking' -> "DIGITAL_BANKING"
  AFeatureNppPayid' -> "NPP_PAYID"
  AFeatureNppEnabled' -> "NPP_ENABLED"
  AFeatureDonateInterest' -> "DONATE_INTEREST"
  AFeatureBillPayment' -> "BILL_PAYMENT"

data AccountFeatureType' =
    AFeatureCardAcess'
  | AFeatureAdditionalCards'
  | AFeatureUnlimitedTxns'
  | AFeatureFreeTxns'
  | AFeatureFreeTxnsAllowance'
  | AFeatureLoyaltyProgram'
  | AFeatureOffset'
  | AFeatureOverdraft'
  | AFeatureRedraw'
  | AFeatureInsurance'
  | AFeatureBalanceTransfers'
  | AFeatureInterestFree'
  | AFeatureInterestFreeTransfers'
  | AFeatureDigitalWallet'
  | AFeatureDigitalBanking'
  | AFeatureNppPayid'
  | AFeatureNppEnabled'
  | AFeatureDonateInterest'
  | AFeatureBillPayment'
  deriving (Eq, Show)

accountFeatureType'Encoder :: Applicative f => Encoder f AccountFeatureType'
accountFeatureType'Encoder = flip contramap E.text accountFeatureType'ToText

accountFeatureTypeToType' :: AccountFeatureType -> AccountFeatureType'
accountFeatureTypeToType' (AFeatureCardAcess {}) = AFeatureCardAcess'
accountFeatureTypeToType' (AFeatureAdditionalCards {}) = AFeatureAdditionalCards'
accountFeatureTypeToType' (AFeatureUnlimitedTxns {}) = AFeatureUnlimitedTxns'
accountFeatureTypeToType' (AFeatureFreeTxns {}) = AFeatureFreeTxns'
accountFeatureTypeToType' (AFeatureFreeTxnsAllowance {}) = AFeatureFreeTxnsAllowance'
accountFeatureTypeToType' (AFeatureLoyaltyProgram {}) = AFeatureLoyaltyProgram'
accountFeatureTypeToType' (AFeatureOffset {}) = AFeatureOffset'
accountFeatureTypeToType' (AFeatureOverdraft {}) = AFeatureOverdraft'
accountFeatureTypeToType' (AFeatureRedraw {}) = AFeatureRedraw'
accountFeatureTypeToType' (AFeatureInsurance {}) = AFeatureInsurance'
accountFeatureTypeToType' (AFeatureBalanceTransfers {}) = AFeatureBalanceTransfers'
accountFeatureTypeToType' (AFeatureInterestFree {}) = AFeatureInterestFree'
accountFeatureTypeToType' (AFeatureInterestFreeTransfers {}) = AFeatureInterestFreeTransfers'
accountFeatureTypeToType' (AFeatureDigitalWallet {}) = AFeatureDigitalWallet'
accountFeatureTypeToType' (AFeatureDigitalBanking {}) = AFeatureDigitalBanking'
accountFeatureTypeToType' (AFeatureNppPayid {}) = AFeatureNppPayid'
accountFeatureTypeToType' (AFeatureNppEnabled {}) = AFeatureNppEnabled'
accountFeatureTypeToType' (AFeatureDonateInterest {}) = AFeatureDonateInterest'
accountFeatureTypeToType' (AFeatureBillPayment {}) = AFeatureBillPayment'

accountFeatureTypeEncoder :: Applicative f => Encoder f AccountFeatureType
accountFeatureTypeEncoder = E.mapLikeObj $ \pc -> do
  case pc of
    AFeatureCardAcess ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureAdditionalCards v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.int v
    AFeatureUnlimitedTxns ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureFreeTxns v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.int v
    AFeatureFreeTxnsAllowance v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    AFeatureLoyaltyProgram v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    AFeatureOffset ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureOverdraft ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureRedraw ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureInsurance v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    AFeatureBalanceTransfers ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureInterestFree v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    AFeatureInterestFreeTransfers v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    AFeatureDigitalWallet v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    AFeatureDigitalBanking ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureNppPayid ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureNppEnabled ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureDonateInterest ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc)
    AFeatureBillPayment v ->
      E.atKey' "featureType" accountFeatureType'Encoder (accountFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB AccountFeatureType where
  mkDecoder = tagOb accountFeatureTypeDecoder
