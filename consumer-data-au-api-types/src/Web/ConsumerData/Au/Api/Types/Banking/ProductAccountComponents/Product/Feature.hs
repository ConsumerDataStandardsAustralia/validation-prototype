{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Feature
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Feature
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


newtype ProductFeatures =
  ProductFeatures { getProductFeatures :: [ProductFeatureType] }
  deriving (Eq, Show)

productFeaturesDecoder :: Monad f => Decoder f ProductFeatures
productFeaturesDecoder = ProductFeatures <$> D.list productFeatureTypeDecoder

productFeaturesEncoder :: Applicative f => Encoder f ProductFeatures
productFeaturesEncoder = getProductFeatures >$< E.list productFeatureTypeEncoder

instance JsonDecode OB ProductFeatures where
  mkDecoder = tagOb productFeaturesDecoder

instance JsonEncode OB ProductFeatures where
  mkEncoder = tagOb productFeaturesEncoder


-- | ProductFeature = ProductFeatureType <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductfeature CDR AU v0.1.0 ProductFeature>
-- WARNING This type is refering to *Product* only property, not as it defined in the above link
-- | Description of the usage of the @featureType@ field as it applies to products. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#productfeaturetypedoc CDR AU v0.1.0 Product Feature Types>
data ProductFeatureType =
    PFeatureCardAcess -- ^ "CARD_ACCESS" A card is available for the product to access funds.
  | PFeatureAdditionalCards Int -- ^ "ADDITIONAL_CARDS" Additional cards can be requested. Use of @additionalValue@ field: The maximum number of additional cards. If no maximum then should be set to null.
  | PFeatureUnlimitedTxns -- ^ "UNLIMITED_TXNS" Unlimited free transactions available.
  | PFeatureFreeTxns Int -- ^ "FREE_TXNS" A set number of free transactions available per month. Use of @additionalValue@ field: The number of free transactions.
  | PFeatureFreeTxnsAllowance AmountString -- ^ "FREE_TXNS_ALLOWANCE" A set amount of transaction fee value that is discounted per month. Use of @additionalValue@ field: The amount of transaction fee discounted (in AUD).
  | PFeatureLoyaltyProgram Text -- ^ "LOYALTY_PROGRAM" A points based loyalty program is available. Use of @additionalValue@ field: Name of the loyalty program.
  | PFeatureOffset-- ^ "OFFSET" An offset account can be connected to the product.
  | PFeatureOverdraft -- ^ "OVERDRAFT" An overdraft can be applied for.
  | PFeatureRedraw -- ^ "REDRAW" Redraw of repaid principal above minimum required is available.
  | PFeatureInsurance Text -- ^ "INSURANCE" Insurance is provided as an additional feature of the product. Use of @additionalValue@ field: Text description of the type of insurance (e.g. Travel Insurance).
  | PFeatureBalanceTransfers -- ^ "BALANCE_TRANSFERS" Balance transfers can be made from the account (eg. for credit cards).
  | PFeatureInterestFree DurationString -- ^ "INTEREST_FREE" Interest free period for purchases. Use of @additionalValue@ field:      Interest free period. Formatted according to ISO 8601 Durations.
  | PFeatureInterestFreeTransfers DurationString -- ^ "INTEREST_FREE_TRANSFERS" Interest free period for balance transfers. Use of @additionalValue@ field:      Interest free period. Formatted according to ISO 8601 Durations.
  | PFeatureDigitalWallet Text -- ^ "DIGITAL_WALLET" A Digital wallet can be attached to the product. Use of @additionalValue@ field: The name or brand of the wallet.
  | PFeatureDigitalBanking -- ^ "DIGITAL_BANKING" Access is available to online banking features for the product.
  | PFeatureNppPayid-- ^ "NPP_PAYID" An account of this product type can be used as the target of an NPP PayID.
  | PFeatureNppEnabled -- ^ "NPP_ENABLED" An account of this product type can be used to receive funds as a result of a BSB/Number based NPP payment.
  | PFeatureDonateInterest -- ^ "DONATE_INTEREST" Indicates that interest generated from the product can be automatically donated to a charity or community group.
  | PFeatureBillPayment Text -- ^ "BILL_PAYMENT" The product can be attached to an automatic budgeting and bill payment service. Use of @additionalValue@ field: Optional name of the service.
  deriving (Show, Eq)

productFeatureTypeDecoder :: Monad f => Decoder f ProductFeatureType
productFeatureTypeDecoder = do
  featureType <- D.atKey "featureType" D.text
  additionalValue <- case featureType of
    "CARD_ACCESS" -> pure PFeatureCardAcess
    "ADDITIONAL_CARDS" -> PFeatureAdditionalCards <$> (additionalValueDecoder D.int)
    "UNLIMITED_TXNS" -> pure PFeatureUnlimitedTxns
    "FREE_TXNS" -> PFeatureFreeTxns <$> (additionalValueDecoder D.int)
    "FREE_TXNS_ALLOWANCE" -> PFeatureFreeTxnsAllowance <$> (additionalValueDecoder amountStringDecoder)
    "LOYALTY_PROGRAM" -> PFeatureLoyaltyProgram <$> (additionalValueDecoder D.text)
    "OFFSET" -> pure PFeatureOffset
    "OVERDRAFT" -> pure PFeatureOverdraft
    "REDRAW" -> pure PFeatureRedraw
    "INSURANCE" -> PFeatureInsurance <$> (additionalValueDecoder D.text)
    "BALANCE_TRANSFERS" -> pure PFeatureBalanceTransfers
    "INTEREST_FREE" -> PFeatureInterestFree <$> (additionalValueDecoder durationStringDecoder)
    "INTEREST_FREE_TRANSFERS" -> PFeatureInterestFreeTransfers <$> (additionalValueDecoder durationStringDecoder)
    "DIGITAL_WALLET" -> PFeatureDigitalWallet <$> (additionalValueDecoder D.text)
    "DIGITAL_BANKING" -> pure PFeatureDigitalBanking
    "NPP_PAYID" -> pure PFeatureNppPayid
    "NPP_ENABLED" -> pure PFeatureNppEnabled
    "DONATE_INTEREST" -> pure PFeatureDonateInterest
    "BILL_PAYMENT" -> PFeatureBillPayment <$> (additionalValueDecoder D.text)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

productFeatureType'ToText :: ProductFeatureType' -> Text
productFeatureType'ToText = \case
  PFeatureCardAcess' -> "CARD_ACCESS"
  PFeatureAdditionalCards' -> "ADDITIONAL_CARDS"
  PFeatureUnlimitedTxns' -> "UNLIMITED_TXNS"
  PFeatureFreeTxns' -> "FREE_TXNS"
  PFeatureFreeTxnsAllowance' -> "FREE_TXNS_ALLOWANCE"
  PFeatureLoyaltyProgram' -> "LOYALTY_PROGRAM"
  PFeatureOffset' -> "OFFSET"
  PFeatureOverdraft' -> "OVERDRAFT"
  PFeatureRedraw' -> "REDRAW"
  PFeatureInsurance' -> "INSURANCE"
  PFeatureBalanceTransfers' -> "BALANCE_TRANSFERS"
  PFeatureInterestFree' -> "INTEREST_FREE"
  PFeatureInterestFreeTransfers' -> "INTEREST_FREE_TRANSFERS"
  PFeatureDigitalWallet' -> "DIGITAL_WALLET"
  PFeatureDigitalBanking' -> "DIGITAL_BANKING"
  PFeatureNppPayid' -> "NPP_PAYID"
  PFeatureNppEnabled' -> "NPP_ENABLED"
  PFeatureDonateInterest' -> "DONATE_INTEREST"
  PFeatureBillPayment' -> "BILL_PAYMENT"

data ProductFeatureType' =
    PFeatureCardAcess'
  | PFeatureAdditionalCards'
  | PFeatureUnlimitedTxns'
  | PFeatureFreeTxns'
  | PFeatureFreeTxnsAllowance'
  | PFeatureLoyaltyProgram'
  | PFeatureOffset'
  | PFeatureOverdraft'
  | PFeatureRedraw'
  | PFeatureInsurance'
  | PFeatureBalanceTransfers'
  | PFeatureInterestFree'
  | PFeatureInterestFreeTransfers'
  | PFeatureDigitalWallet'
  | PFeatureDigitalBanking'
  | PFeatureNppPayid'
  | PFeatureNppEnabled'
  | PFeatureDonateInterest'
  | PFeatureBillPayment'
  deriving (Eq, Show)

productFeatureType'Encoder :: Applicative f => Encoder f ProductFeatureType'
productFeatureType'Encoder = flip contramap E.text productFeatureType'ToText

productFeatureTypeToType' :: ProductFeatureType -> ProductFeatureType'
productFeatureTypeToType' (PFeatureCardAcess {}) = PFeatureCardAcess'
productFeatureTypeToType' (PFeatureAdditionalCards {}) = PFeatureAdditionalCards'
productFeatureTypeToType' (PFeatureUnlimitedTxns {}) = PFeatureUnlimitedTxns'
productFeatureTypeToType' (PFeatureFreeTxns {}) = PFeatureFreeTxns'
productFeatureTypeToType' (PFeatureFreeTxnsAllowance {}) = PFeatureFreeTxnsAllowance'
productFeatureTypeToType' (PFeatureLoyaltyProgram {}) = PFeatureLoyaltyProgram'
productFeatureTypeToType' (PFeatureOffset {}) = PFeatureOffset'
productFeatureTypeToType' (PFeatureOverdraft {}) = PFeatureOverdraft'
productFeatureTypeToType' (PFeatureRedraw {}) = PFeatureRedraw'
productFeatureTypeToType' (PFeatureInsurance {}) = PFeatureInsurance'
productFeatureTypeToType' (PFeatureBalanceTransfers {}) = PFeatureBalanceTransfers'
productFeatureTypeToType' (PFeatureInterestFree {}) = PFeatureInterestFree'
productFeatureTypeToType' (PFeatureInterestFreeTransfers {}) = PFeatureInterestFreeTransfers'
productFeatureTypeToType' (PFeatureDigitalWallet {}) = PFeatureDigitalWallet'
productFeatureTypeToType' (PFeatureDigitalBanking {}) = PFeatureDigitalBanking'
productFeatureTypeToType' (PFeatureNppPayid {}) = PFeatureNppPayid'
productFeatureTypeToType' (PFeatureNppEnabled {}) = PFeatureNppEnabled'
productFeatureTypeToType' (PFeatureDonateInterest {}) = PFeatureDonateInterest'
productFeatureTypeToType' (PFeatureBillPayment {}) = PFeatureBillPayment'

productFeatureTypeEncoder :: Applicative f => Encoder f ProductFeatureType
productFeatureTypeEncoder = E.mapLikeObj $ \pc -> do
  case pc of
    PFeatureCardAcess ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureAdditionalCards v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.int v
    PFeatureUnlimitedTxns ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureFreeTxns v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.int v
    PFeatureFreeTxnsAllowance v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    PFeatureLoyaltyProgram v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PFeatureOffset ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureOverdraft ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureRedraw ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureInsurance v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PFeatureBalanceTransfers ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureInterestFree v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    PFeatureInterestFreeTransfers v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    PFeatureDigitalWallet v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PFeatureDigitalBanking ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureNppPayid ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureNppEnabled ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureDonateInterest ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc)
    PFeatureBillPayment v ->
      E.atKey' "featureType" productFeatureType'Encoder (productFeatureTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB ProductFeatureType where
  mkDecoder = tagOb productFeatureTypeDecoder
