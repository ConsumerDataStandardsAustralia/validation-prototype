{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.DepositRate
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.DepositRate
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

import           Waargonaut.Helpers         (fromKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (DurationString, RateString, durationStringDecoder, durationStringEncoder,
    rateStringDecoder, rateStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype ProductDepositRates =
  ProductDepositRates { getProductDepositRates :: [ProductDepositRate] }
  deriving (Eq, Show)

productDepositRatesDecoder :: Monad f => Decoder f ProductDepositRates
productDepositRatesDecoder = ProductDepositRates <$> D.list productDepositRateDecoder

productDepositRatesEncoder :: Applicative f => Encoder f ProductDepositRates
productDepositRatesEncoder = getProductDepositRates >$< E.list productDepositRateEncoder

instance JsonDecode OB ProductDepositRates where
  mkDecoder = tagOb productDepositRatesDecoder

instance JsonEncode OB ProductDepositRates where
  mkEncoder = tagOb productDepositRatesEncoder



-- | ProductDepositRate <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductdepositerate CDR AU v0.1.0 ProductDepositRate>
data ProductDepositRate = ProductDepositRate
-- WARNING This type is refering to *Product* only property, not as it defined in the above link
  { _productDepositRateDepositRateType   :: ProductDepositRateType -- ^ The type of rate (base, bonus, etc). See the next section for an overview of valid values and their meaning
-- WARNING
  , _productDepositRateRate              :: RateString -- ^ The rate to be applied
  , _productDepositRateAdditionalInfo    :: Maybe Text -- ^ Display text providing more information on the rate
  , _productDepositRateAdditionalInfoUri :: Maybe URI -- ^ Link to a web page with more information on this fee
  } deriving (Show, Eq)

productDepositRateDecoder :: Monad f => Decoder f ProductDepositRate
productDepositRateDecoder = D.withCursor $ \c -> do
  o <- D.down c
  ProductDepositRate
    <$> D.focus productDepositRateTypeDecoder o
    <*> D.fromKey "rate" rateStringDecoder o
    <*> fromKeyOptional' "additionalInfo" D.text o
    <*> fromKeyOptional' "additionalInfoUri" uriDecoder o

instance JsonDecode OB ProductDepositRate where
  mkDecoder = tagOb productDepositRateDecoder

productDepositRateEncoder :: Applicative f => Encoder f ProductDepositRate
productDepositRateEncoder = E.mapLikeObj $ \p ->
  productDepositRateTypeFields (_productDepositRateDepositRateType p) .
  E.atKey' "rate" rateStringEncoder (_productDepositRateRate p) .
  maybeOrAbsentE "additionalInfo" E.text (_productDepositRateAdditionalInfo p) .
  maybeOrAbsentE "additionalInfoUri" uriEncoder (_productDepositRateAdditionalInfoUri p)

instance JsonEncode OB ProductDepositRate where
  mkEncoder = tagOb productDepositRateEncoder


-- | Description of the usage of the @depositRateType@ field as it applies to products. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#productdepositratetypedoc CDR AU v0.1.0 Product Deposit Rate Types>
data ProductDepositRateType =
    PDepositRateTypeFixed DurationString -- ^ "FIXED" Fixed rate for a period of time. Use of @additionalValue@ field: The period of time fixed. Formatted according to <https://en.wikipedia.org/wiki/ISO_8601#Durations ISO 8601 Durations>.
  | PDepositRateTypeBonus Text -- ^ "BONUS" A bonus rate available by meeting a specific criteria. Use of @additionalValue@ field: A description of the criteria to obtain the bonus.
  | PDepositRateTypeBundleBonus Text -- ^ "BUNDLE_BONUS" A bonus rate obtained by originating a bundle instead of a standalone product. Use of @additionalValue@ field: The name of the bundle.
  | PDepositRateTypeVariable -- ^ "VARIABLE" A variable base rate for the product
  | PDepositRateTypeIntroductory DurationString -- ^ "INTRODUCTORY" An introductory bonus that will expire after a set period. Use of @additionalValue@ field: The period of time for the introductory rate. Formatted according to <https://en.wikipedia.org/wiki/ISO_8601#Durations ISO 8601 Durations>.
  deriving (Show, Eq)

productDepositRateTypeDecoder :: Monad f => Decoder f ProductDepositRateType
productDepositRateTypeDecoder = D.withCursor $ \c -> do
  -- D.focus D.text c >>= \case
  o <- D.down c
  depositRateType <- D.fromKey "depositRateType" D.text o
  additionalValue <- case depositRateType of
    "FIXED" -> PDepositRateTypeFixed <$> (additionalValueDecoder durationStringDecoder o)
    "BONUS" -> PDepositRateTypeBonus <$> (additionalValueDecoder D.text o)
    "BUNDLE_BONUS" -> PDepositRateTypeBundleBonus <$> (additionalValueDecoder D.text o)
    "VARIABLE" -> pure PDepositRateTypeVariable
    "INTRODUCTORY" -> PDepositRateTypeIntroductory <$> (additionalValueDecoder durationStringDecoder o)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

productDepositRateType'ToText :: ProductDepositRateType' -> Text
productDepositRateType'ToText = \case
  PDepositRateTypeFixed' -> "FIXED"
  PDepositRateTypeBonus' -> "BONUS"
  PDepositRateTypeBundleBonus' -> "BUNDLE_BONUS"
  PDepositRateTypeVariable' -> "VARIABLE"
  PDepositRateTypeIntroductory' -> "INTRODUCTORY"

data ProductDepositRateType' =
    PDepositRateTypeFixed'
  | PDepositRateTypeBonus'
  | PDepositRateTypeBundleBonus'
  | PDepositRateTypeVariable'
  | PDepositRateTypeIntroductory'
  deriving (Eq, Show)

productDepositRateType'Encoder :: Applicative f => Encoder f ProductDepositRateType'
productDepositRateType'Encoder = flip contramap E.text productDepositRateType'ToText

productDepositRateTypeToType' :: ProductDepositRateType -> ProductDepositRateType'
productDepositRateTypeToType' (PDepositRateTypeFixed {}) = PDepositRateTypeFixed'
productDepositRateTypeToType' (PDepositRateTypeBonus {}) = PDepositRateTypeBonus'
productDepositRateTypeToType' (PDepositRateTypeBundleBonus {}) = PDepositRateTypeBundleBonus'
productDepositRateTypeToType' (PDepositRateTypeVariable {}) = PDepositRateTypeVariable'
productDepositRateTypeToType' (PDepositRateTypeIntroductory {}) = PDepositRateTypeIntroductory'

productDepositRateTypeFields :: (Monoid ws, Semigroup ws) => ProductDepositRateType -> MapLikeObj ws Json -> MapLikeObj ws Json
productDepositRateTypeFields pc =
-- productDepositRateTypeEncoder :: Applicative f => Encoder f ProductDepositRateType
-- productDepositRateTypeEncoder = E.mapLikeObj $ \pc -> do
  case pc of
    PDepositRateTypeFixed v ->
      E.atKey' "depositRateType" productDepositRateType'Encoder (productDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v
    PDepositRateTypeBonus v ->
      E.atKey' "depositRateType" productDepositRateType'Encoder (productDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PDepositRateTypeBundleBonus v ->
      E.atKey' "depositRateType" productDepositRateType'Encoder (productDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PDepositRateTypeVariable ->
      E.atKey' "depositRateType" productDepositRateType'Encoder (productDepositRateTypeToType' pc)
    PDepositRateTypeIntroductory v ->
      E.atKey' "depositRateType" productDepositRateType'Encoder (productDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" durationStringEncoder v

instance JsonDecode OB ProductDepositRateType where
  mkDecoder = tagOb productDepositRateTypeDecoder
