{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.DepositRate
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.DepositRate
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
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (DateTimeString, RateString, dateTimeStringDecoder,
    dateTimeStringEncoder, rateStringDecoder, rateStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype AccountDepositRates =
  AccountDepositRates { getAccountDepositRates :: [AccountDepositRate] }
  deriving (Eq, Show)

accountDepositRatesDecoder :: Monad f => Decoder f AccountDepositRates
accountDepositRatesDecoder = AccountDepositRates <$> D.list accountDepositRateDecoder

accountDepositRatesEncoder :: Applicative f => Encoder f AccountDepositRates
accountDepositRatesEncoder = getAccountDepositRates >$< E.list accountDepositRateEncoder

instance JsonDecode OB AccountDepositRates where
  mkDecoder = tagOb accountDepositRatesDecoder

instance JsonEncode OB AccountDepositRates where
  mkEncoder = tagOb accountDepositRatesEncoder



-- | AccountDepositRate <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductdepositerate CDR AU v0.1.0 ProductDepositRate>
-- WARNING This type is refering to *Account* only property, not as it defined in the above link
data AccountDepositRate = AccountDepositRate
  { _accountDepositRateDepositRateType   :: AccountDepositRateType -- ^ The type of rate (base, bonus, etc). See the next section for an overview of valid values and their meaning
  , _accountDepositRateRate              :: RateString -- ^ The rate to be applied
  , _accountDepositRateAdditionalInfo    :: Maybe Text -- ^ Display text providing more information on the rate
  , _accountDepositRateAdditionalInfoUri :: Maybe URI -- ^ Link to a web page with more information on this fee
  } deriving (Show, Eq)

accountDepositRateDecoder :: Monad f => Decoder f AccountDepositRate
accountDepositRateDecoder =
  AccountDepositRate
    <$> accountDepositRateTypeDecoder
    <*> D.atKey "rate" rateStringDecoder
    <*> atKeyOptional' "additionalInfo" D.text
    <*> atKeyOptional' "additionalInfoUri" uriDecoder

instance JsonDecode OB AccountDepositRate where
  mkDecoder = tagOb accountDepositRateDecoder

accountDepositRateEncoder :: Applicative f => Encoder f AccountDepositRate
accountDepositRateEncoder = E.mapLikeObj $ \p ->
  accountDepositRateTypeFields (_accountDepositRateDepositRateType p) .
  E.atKey' "rate" rateStringEncoder (_accountDepositRateRate p) .
  maybeOrAbsentE "additionalInfo" E.text (_accountDepositRateAdditionalInfo p) .
  maybeOrAbsentE "additionalInfoUri" uriEncoder (_accountDepositRateAdditionalInfoUri p)

instance JsonEncode OB AccountDepositRate where
  mkEncoder = tagOb accountDepositRateEncoder


-- | Description of the usage of the @depositRateType@ field as it applies to accounts. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#accountdepositratetypedoc CDR AU v0.1.0 Account Deposit Rate Types>
data AccountDepositRateType =
-- different type for accounts
    ADepositRateTypeFixed DateTimeString -- ^ "FIXED" Fixed rate for a period of time. Use of @additionalValue@ field: DateTimeString representing when the fixed rate will expire.
  | ADepositRateTypeBonus Text -- ^ "BONUS" A bonus rate available by meeting a specific criteria. Use of @additionalValue@ field: A description of the criteria to obtain the bonus.
  | ADepositRateTypeBundleBonus Text -- ^ "BUNDLE_BONUS" A bonus rate obtained by originating a bundle instead of a standalone product. Use of @additionalValue@ field: The name of the bundle.
  | ADepositRateTypeVariable -- ^ "VARIABLE" A variable base rate for the product
-- different type for accounts
  | ADepositRateTypeIntroductory DateTimeString -- ^ "INTRODUCTORY" An introductory bonus that will expire after a set period. Use of @additionalValue@ field: DateTimeString representing when the introductory rate will expire.
  deriving (Eq, Show)

accountDepositRateTypeDecoder :: Monad f => Decoder f AccountDepositRateType
accountDepositRateTypeDecoder = do
  depositRateType <- D.atKey "depositRateType" D.text
  additionalValue <- case depositRateType of
    "FIXED" -> ADepositRateTypeFixed <$> (additionalValueDecoder dateTimeStringDecoder)
    "BONUS" -> ADepositRateTypeBonus <$> (additionalValueDecoder D.text)
    "BUNDLE_BONUS" -> ADepositRateTypeBundleBonus <$> (additionalValueDecoder D.text)
    "VARIABLE" -> pure ADepositRateTypeVariable
    "INTRODUCTORY" -> ADepositRateTypeIntroductory <$> (additionalValueDecoder dateTimeStringDecoder)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

accountDepositRateType'ToText :: AccountDepositRateType' -> Text
accountDepositRateType'ToText = \case
  ADepositRateTypeFixed' -> "FIXED"
  ADepositRateTypeBonus' -> "BONUS"
  ADepositRateTypeBundleBonus' -> "BUNDLE_BONUS"
  ADepositRateTypeVariable' -> "VARIABLE"
  ADepositRateTypeIntroductory' -> "INTRODUCTORY"

data AccountDepositRateType' =
    ADepositRateTypeFixed'
  | ADepositRateTypeBonus'
  | ADepositRateTypeBundleBonus'
  | ADepositRateTypeVariable'
  | ADepositRateTypeIntroductory'
  deriving (Eq, Show)

accountDepositRateType'Encoder :: Applicative f => Encoder f AccountDepositRateType'
accountDepositRateType'Encoder = flip contramap E.text accountDepositRateType'ToText

accountDepositRateTypeToType' :: AccountDepositRateType -> AccountDepositRateType'
accountDepositRateTypeToType' (ADepositRateTypeFixed {}) = ADepositRateTypeFixed'
accountDepositRateTypeToType' (ADepositRateTypeBonus {}) = ADepositRateTypeBonus'
accountDepositRateTypeToType' (ADepositRateTypeBundleBonus {}) = ADepositRateTypeBundleBonus'
accountDepositRateTypeToType' (ADepositRateTypeVariable {}) = ADepositRateTypeVariable'
accountDepositRateTypeToType' (ADepositRateTypeIntroductory {}) = ADepositRateTypeIntroductory'

accountDepositRateTypeFields :: (Monoid ws, Semigroup ws) => AccountDepositRateType -> MapLikeObj ws Json -> MapLikeObj ws Json
accountDepositRateTypeFields pc =
  case pc of
    ADepositRateTypeFixed v ->
      E.atKey' "depositRateType" accountDepositRateType'Encoder (accountDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" dateTimeStringEncoder v
    ADepositRateTypeBonus v ->
      E.atKey' "depositRateType" accountDepositRateType'Encoder (accountDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ADepositRateTypeBundleBonus v ->
      E.atKey' "depositRateType" accountDepositRateType'Encoder (accountDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    ADepositRateTypeVariable ->
      E.atKey' "depositRateType" accountDepositRateType'Encoder (accountDepositRateTypeToType' pc)
    ADepositRateTypeIntroductory v ->
      E.atKey' "depositRateType" accountDepositRateType'Encoder (accountDepositRateTypeToType' pc) .
      E.atKey' "additionalValue" dateTimeStringEncoder v

instance JsonDecode OB AccountDepositRateType where
  mkDecoder = tagOb accountDepositRateTypeDecoder
