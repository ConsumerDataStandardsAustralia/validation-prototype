{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.LendingRate
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.LendingRate
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
    (DateTimeString, RateString, dateTimeStringDecoder,
    dateTimeStringEncoder, rateStringDecoder, rateStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype AccountLendingRates =
  AccountLendingRates { getAccountLendingRates :: [AccountLendingRate] }
  deriving (Eq, Show)

accountLendingRatesDecoder :: Monad f => Decoder f AccountLendingRates
accountLendingRatesDecoder = AccountLendingRates <$> D.list accountLendingRateDecoder

accountLendingRatesEncoder :: Applicative f => Encoder f AccountLendingRates
accountLendingRatesEncoder = getAccountLendingRates >$< E.list accountLendingRateEncoder

instance JsonDecode OB AccountLendingRates where
  mkDecoder = tagOb accountLendingRatesDecoder

instance JsonEncode OB AccountLendingRates where
  mkEncoder = tagOb accountLendingRatesEncoder



-- | AccountLendingRate <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductlendingrate CDR AU v0.1.0 ProductLendingRate>
-- WARNING This type is refering to *Account* only property, not as it defined in the above link
data AccountLendingRate = AccountLendingRate
  { _accountLendingRateLendingRateType   :: AccountLendingRateType -- ^ The type of rate (base, bonus, etc). See the next section for an overview of valid values and their meaning
-- WARNING
  , _accountLendingRateRate              :: RateString -- ^ The rate to be applied
  , _accountLendingRateAdditionalInfo    :: Maybe Text -- ^ Display text providing more information on the rate
  , _accountLendingRateAdditionalInfoUri :: Maybe URI -- ^ Link to a web page with more information on this fee
  } deriving (Show, Eq)

accountLendingRateDecoder :: Monad f => Decoder f AccountLendingRate
accountLendingRateDecoder = D.withCursor $ \c -> do
  o <- D.down c
  AccountLendingRate
    <$> D.focus accountLendingRateTypeDecoder o
    <*> D.fromKey "rate" rateStringDecoder o
    <*> D.fromKey "additionalInfo" (D.maybeOrNull D.text) o
    <*> D.fromKey "additionalInfoUri" (D.maybeOrNull uriDecoder) o

instance JsonDecode OB AccountLendingRate where
  mkDecoder = tagOb accountLendingRateDecoder

accountLendingRateEncoder :: Applicative f => Encoder f AccountLendingRate
accountLendingRateEncoder = E.mapLikeObj $ \p ->
  accountLendingRateTypeFields (_accountLendingRateLendingRateType p) .
  E.atKey' "rate" rateStringEncoder (_accountLendingRateRate p) .
  E.atKey' "additionalInfo" (E.maybeOrNull E.text) (_accountLendingRateAdditionalInfo p) .
  E.atKey' "additionalInfoUri" (E.maybeOrNull uriEncoder) (_accountLendingRateAdditionalInfoUri p)

instance JsonEncode OB AccountLendingRate where
  mkEncoder = tagOb accountLendingRateEncoder


-- | Description of the usage of the @lendingRateType@ field as it applies to accounts. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#accountlendingratetypedoc CDR AU v0.1.0 Account Lending Rate Types>
data AccountLendingRateType =
-- different type for accounts
    ALendingRateFixed DateTimeString -- ^ "FIXED" Fixed rate for a period of time. Use of @additionalValue@ field: DateTimeString representing when the fixed rate will expire.
-- different type for accounts
  | ALendingRateIntroductory DateTimeString -- ^ "INTRODUCTORY" An introductory discount that will expire after a set period. Use of @additionalValue@ field: DateTimeString representing when the introductory rate will expire
  | ALendingRateDiscount Text -- ^ "DISCOUNT" A specific discount rate that may be applied. A discount rate reduces the interest payable. Use of @additionalValue@ field: Description of the discount rate that is applicable.
  | ALendingRatePenalty Text -- ^ "PENALTY" A specific penalty rate that may be applied. A penalty rate increases the interest payable. Use of @additionalValue@ field: Description of the penalty rate that is applicable.
  | ALendingRateBundleDiscount Text -- ^ "BUNDLE_DISCOUNT" A discount rate obtained by originating a bundle instead of a standalone product. Use of @additionalValue@ field: The name of the bundle.
  | ALendingRateFloating Text -- ^ "FLOATING" A floating rate is relatively fixed but still adjusts under specific circumstances. Use of @additionalValue@ field: Details of the float parameters.
  | ALendingRateMarketLinked Text -- ^ "MARKET_LINKED" A rate that is linked to a specific market, commodity or asset class. Use of @additionalValue@ field: Details of the market linkage.
  | ALendingRateCashAdvance -- ^ "CASH_ADVANCE" Specific rate applied to case advances from the account.
  | ALendingRateVariable -- ^ "VARIABLE" A variable base rate for the product.
  | ALendingRateComparison Text -- ^ "COMPARISON" A comparison rate for the product. Use of @additionalValue@ field:
  deriving (Show, Eq)

accountLendingRateTypeDecoder :: Monad f => Decoder f AccountLendingRateType
accountLendingRateTypeDecoder = D.withCursor $ \c -> do
  -- D.focus D.text c >>= \case
  o <- D.down c
  lendingRateType <- D.fromKey "lendingRateType" D.text o
  additionalValue <- case lendingRateType of
    "FIXED" -> ALendingRateFixed <$> (additionalValueDecoder dateTimeStringDecoder o)
    "INTRODUCTORY" -> ALendingRateIntroductory <$> (additionalValueDecoder dateTimeStringDecoder o)
    "DISCOUNT" -> ALendingRateDiscount <$> (additionalValueDecoder D.text o)
    "PENALTY" -> ALendingRatePenalty <$> (additionalValueDecoder D.text o)
    "BUNDLE_DISCOUNT" -> ALendingRateBundleDiscount <$> (additionalValueDecoder D.text o)
    "FLOATING" -> ALendingRateFloating <$> (additionalValueDecoder D.text o)
    "MARKET_LINKED" -> ALendingRateMarketLinked <$> (additionalValueDecoder D.text o)
    "CASH_ADVANCE" -> pure ALendingRateCashAdvance
    "VARIABLE" -> pure ALendingRateVariable
    "COMPARISON" -> ALendingRateComparison <$> (additionalValueDecoder D.text o)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

accountLendingRateType'ToText :: AccountLendingRateType' -> Text
accountLendingRateType'ToText = \case
  ALendingRateFixed' -> "FIXED"
  ALendingRateIntroductory' -> "INTRODUCTORY"
  ALendingRateDiscount' -> "DISCOUNT"
  ALendingRatePenalty' -> "PENALTY"
  ALendingRateBundleDiscount' -> "BUNDLE_DISCOUNT"
  ALendingRateFloating' -> "FLOATING"
  ALendingRateMarketLinked' -> "MARKET_LINKED"
  ALendingRateCashAdvance' -> "CASH_ADVANCE"
  ALendingRateVariable' -> "VARIABLE"
  ALendingRateComparison' -> "COMPARISON"

data AccountLendingRateType' =
    ALendingRateFixed'
  | ALendingRateIntroductory'
  | ALendingRateDiscount'
  | ALendingRatePenalty'
  | ALendingRateBundleDiscount'
  | ALendingRateFloating'
  | ALendingRateMarketLinked'
  | ALendingRateCashAdvance'
  | ALendingRateVariable'
  | ALendingRateComparison'
  deriving (Eq, Show)

accountLendingRateType'Encoder :: Applicative f => Encoder f AccountLendingRateType'
accountLendingRateType'Encoder = flip contramap E.text accountLendingRateType'ToText

accountLendingRateTypeToType' :: AccountLendingRateType -> AccountLendingRateType'
accountLendingRateTypeToType' (ALendingRateFixed {}) = ALendingRateFixed'
accountLendingRateTypeToType' (ALendingRateIntroductory {}) = ALendingRateIntroductory'
accountLendingRateTypeToType' (ALendingRateDiscount {}) = ALendingRateDiscount'
accountLendingRateTypeToType' (ALendingRatePenalty {}) = ALendingRatePenalty'
accountLendingRateTypeToType' (ALendingRateBundleDiscount {}) = ALendingRateBundleDiscount'
accountLendingRateTypeToType' (ALendingRateFloating {}) = ALendingRateFloating'
accountLendingRateTypeToType' (ALendingRateMarketLinked {}) = ALendingRateMarketLinked'
accountLendingRateTypeToType' (ALendingRateCashAdvance {}) = ALendingRateCashAdvance'
accountLendingRateTypeToType' (ALendingRateVariable {}) = ALendingRateVariable'
accountLendingRateTypeToType' (ALendingRateComparison {}) = ALendingRateComparison'


accountLendingRateTypeFields :: (Monoid ws, Semigroup ws) => AccountLendingRateType -> MapLikeObj ws Json -> MapLikeObj ws Json
accountLendingRateTypeFields pc =
-- accountLendingRateTypeEncoder :: Applicative f => Encoder f AccountLendingRateType
-- accountLendingRateTypeEncoder = E.mapLikeObj $ \pc -> do
  case pc of
    ALendingRateFixed v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" dateTimeStringEncoder v
    ALendingRateIntroductory v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" dateTimeStringEncoder v

    ALendingRateDiscount v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ALendingRatePenalty v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ALendingRateBundleDiscount v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ALendingRateFloating v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ALendingRateMarketLinked v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ALendingRateCashAdvance ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc)
    ALendingRateVariable ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc)
    ALendingRateComparison v ->
      E.atKey' "lendingRateType" accountLendingRateType'Encoder (accountLendingRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB AccountLendingRateType where
  mkDecoder = tagOb accountLendingRateTypeDecoder
