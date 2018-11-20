{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.LendingRate
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.LendingRate
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

import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (DurationString, RateString, durationStringDecoder, durationStringEncoder,
    rateStringDecoder, rateStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype ProductLendingRates =
  ProductLendingRates { getProductLendingRates :: [ProductLendingRate] }
  deriving (Eq, Show)

productLendingRatesDecoder :: Monad f => Decoder f ProductLendingRates
productLendingRatesDecoder = ProductLendingRates <$> D.list productLendingRateDecoder

productLendingRatesEncoder :: Applicative f => Encoder f ProductLendingRates
productLendingRatesEncoder = getProductLendingRates >$< E.list productLendingRateEncoder

instance JsonDecode OB ProductLendingRates where
  mkDecoder = tagOb productLendingRatesDecoder

instance JsonEncode OB ProductLendingRates where
  mkEncoder = tagOb productLendingRatesEncoder



-- | ProductLendingRate <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductlendingrate CDR AU v0.1.0 ProductLendingRate>
-- WARNING This type is refering to *Product* only property, not as it defined in the above link
data ProductLendingRate = ProductLendingRate
  { _productLendingRateLendingRateType   :: ProductLendingRateType -- ^ The type of rate (base, bonus, etc). See the next section for an overview of valid values and their meaning
-- WARNING
  , _productLendingRateRate              :: RateString -- ^ The rate to be applied
  , _productLendingRateAdditionalInfo    :: Maybe Text -- ^ Display text providing more information on the rate
  , _productLendingRateAdditionalInfoUri :: Maybe URI -- ^ Link to a web page with more information on this fee
  } deriving (Show, Eq)

productLendingRateDecoder :: Monad f => Decoder f ProductLendingRate
productLendingRateDecoder = D.withCursor $ \c -> do
  o <- D.down c
  ProductLendingRate
    <$> (D.focus productLendingRateTypeDecoder o)
    <*> (D.fromKey "rate" rateStringDecoder o)
    <*> (D.try $ D.fromKey "additionalInfo" D.text o)
    <*> (D.try $ D.fromKey "additionalInfoUri" uriDecoder o)

instance JsonDecode OB ProductLendingRate where
  mkDecoder = tagOb productLendingRateDecoder

productLendingRateEncoder :: Applicative f => Encoder f ProductLendingRate
productLendingRateEncoder = E.mapLikeObj $ \p ->
  productLendingRateTypeFields (_productLendingRateLendingRateType p) .
  E.atKey' "rate" rateStringEncoder (_productLendingRateRate p) .
  E.atKey' "additionalInfo" (E.maybeOrNull E.text) (_productLendingRateAdditionalInfo p) .
  E.atKey' "additionalInfoUri" (E.maybeOrNull uriEncoder) (_productLendingRateAdditionalInfoUri p)

instance JsonEncode OB ProductLendingRate where
  mkEncoder = tagOb productLendingRateEncoder


-- | Description of the usage of the @lendingRateType@ field as it applies to products. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#productlendingratetypedoc CDR AU v0.1.0 Product Lending Rate Types>
data ProductLendingRateType =
    PLendingRateFixed DurationString -- ^ "FIXED" Fixed rate for a period of time. Use of @additionalValue@ field: The period of time fixed. Formatted according to ISO 8601 Durations.
  | PLendingRateIntroductory DurationString -- ^ "INTRODUCTORY" An introductory discount that will expire after a set period. Use of @additionalValue@ field: The period of time for the introductory rate. Formatted according to ISO 8601 Durations.
  | PLendingRateDiscount Text -- ^ "DISCOUNT" A specific discount rate that may be applied. A discount rate reduces the interest payable. Use of @additionalValue@ field: Description of the discount rate that is applicable.
  | PLendingRatePenalty Text -- ^ "PENALTY" A specific penalty rate that may be applied. A penalty rate increases the interest payable. Use of @additionalValue@ field: Description of the penalty rate that is applicable.
  | PLendingRateBundleDiscount Text -- ^ "BUNDLE_DISCOUNT" A discount rate obtained by originating a bundle instead of a standalone product. Use of @additionalValue@ field: The name of the bundle.
  | PLendingRateFloating Text -- ^ "FLOATING" A floating rate is relatively fixed but still adjusts under specific circumstances. Use of @additionalValue@ field: Details of the float parameters.
  | PLendingRateMarketLinked Text -- ^ "MARKET_LINKED" A rate that is linked to a specific market, commodity or asset class. Use of @additionalValue@ field: Details of the market linkage.
  | PLendingRateCashAdvance -- ^ "CASH_ADVANCE" Specific rate applied to case advances from the account.
  | PLendingRateVariable -- ^ "VARIABLE" A variable base rate for the product.
  | PLendingRateComparison Text -- ^ "COMPARISON" A comparison rate for the product. Use of @additionalValue@ field:
  deriving (Show, Eq)

productLendingRateTypeDecoder :: Monad f => Decoder f ProductLendingRateType
productLendingRateTypeDecoder = D.withCursor $ \c -> do
  -- D.focus D.text c >>= \case
  o <- D.down c
  lendingRateType <- D.fromKey "lendingRateType" D.text o
  additionalValue <- case lendingRateType of
    "FIXED" -> PLendingRateFixed <$> (additionalValueDecoder durationStringDecoder o)
    "INTRODUCTORY" -> PLendingRateIntroductory <$> (additionalValueDecoder durationStringDecoder o)
    "DISCOUNT" -> PLendingRateDiscount <$> (additionalValueDecoder D.text o)
    "PENALTY" -> PLendingRatePenalty <$> (additionalValueDecoder D.text o)
    "BUNDLE_DISCOUNT" -> PLendingRateBundleDiscount <$> (additionalValueDecoder D.text o)
    "FLOATING" -> PLendingRateFloating <$> (additionalValueDecoder D.text o)
    "MARKET_LINKED" -> PLendingRateMarketLinked <$> (additionalValueDecoder D.text o)
    "CASH_ADVANCE" -> pure PLendingRateCashAdvance
    "VARIABLE" -> pure PLendingRateVariable
    "COMPARISON" -> PLendingRateComparison <$> (additionalValueDecoder D.text o)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

productLendingRateType'ToText :: ProductLendingRateType' -> Text
productLendingRateType'ToText = \case
  PLendingRateFixed' -> "FIXED"
  PLendingRateIntroductory' -> "INTRODUCTORY"
  PLendingRateDiscount' -> "DISCOUNT"
  PLendingRatePenalty' -> "PENALTY"
  PLendingRateBundleDiscount' -> "BUNDLE_DISCOUNT"
  PLendingRateFloating' -> "FLOATING"
  PLendingRateMarketLinked' -> "MARKET_LINKED"
  PLendingRateCashAdvance' -> "CASH_ADVANCE"
  PLendingRateVariable' -> "VARIABLE"
  PLendingRateComparison' -> "COMPARISON"

data ProductLendingRateType' =
    PLendingRateFixed'
  | PLendingRateIntroductory'
  | PLendingRateDiscount'
  | PLendingRatePenalty'
  | PLendingRateBundleDiscount'
  | PLendingRateFloating'
  | PLendingRateMarketLinked'
  | PLendingRateCashAdvance'
  | PLendingRateVariable'
  | PLendingRateComparison'
  deriving (Eq, Show)

productLendingRateType'Encoder :: Applicative f => Encoder f ProductLendingRateType'
productLendingRateType'Encoder = flip contramap E.text productLendingRateType'ToText

productLendingRateTypeToType' :: ProductLendingRateType -> ProductLendingRateType'
productLendingRateTypeToType' (PLendingRateFixed {}) = PLendingRateFixed'
productLendingRateTypeToType' (PLendingRateIntroductory {}) = PLendingRateIntroductory'
productLendingRateTypeToType' (PLendingRateDiscount {}) = PLendingRateDiscount'
productLendingRateTypeToType' (PLendingRatePenalty {}) = PLendingRatePenalty'
productLendingRateTypeToType' (PLendingRateBundleDiscount {}) = PLendingRateBundleDiscount'
productLendingRateTypeToType' (PLendingRateFloating {}) = PLendingRateFloating'
productLendingRateTypeToType' (PLendingRateMarketLinked {}) = PLendingRateMarketLinked'
productLendingRateTypeToType' (PLendingRateCashAdvance {}) = PLendingRateCashAdvance'
productLendingRateTypeToType' (PLendingRateVariable {}) = PLendingRateVariable'
productLendingRateTypeToType' (PLendingRateComparison {}) = PLendingRateComparison'


productLendingRateTypeFields :: (Monoid ws, Semigroup ws) => ProductLendingRateType -> MapLikeObj ws Json -> MapLikeObj ws Json
productLendingRateTypeFields pc =
-- productLendingRateTypeEncoder :: Applicative f => Encoder f ProductLendingRateType
-- productLendingRateTypeEncoder = E.mapLikeObj $ \pc -> do
  case pc of
    PLendingRateFixed v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    PLendingRateIntroductory v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v

    PLendingRateDiscount v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PLendingRatePenalty v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PLendingRateBundleDiscount v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PLendingRateFloating v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PLendingRateMarketLinked v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PLendingRateCashAdvance ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc)
    PLendingRateVariable ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc)
    PLendingRateComparison v ->
      E.atKey' "lendingRateType" productLendingRateType'Encoder (productLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB ProductLendingRateType where
  mkDecoder = tagOb productLendingRateTypeDecoder
