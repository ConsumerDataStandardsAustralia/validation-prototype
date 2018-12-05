{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Eligibility
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Eligibility
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
    (AmountString, amountStringDecoder, amountStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype ProductEligibilities =
  ProductEligibilities { getProductEligibilities :: [ProductEligibility] }
  deriving (Eq, Show)

productEligibilitiesDecoder :: Monad f => Decoder f ProductEligibilities
productEligibilitiesDecoder = ProductEligibilities <$> D.list productEligibilityDecoder

productEligibilitiesEncoder :: Applicative f => Encoder f ProductEligibilities
productEligibilitiesEncoder = getProductEligibilities >$< E.list productEligibilityEncoder

instance JsonDecode OB ProductEligibilities where
  mkDecoder = tagOb productEligibilitiesDecoder

instance JsonEncode OB ProductEligibilities where
  mkEncoder = tagOb productEligibilitiesEncoder


-- | ProductEligibility <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproducteligibility CDR AU v0.1.0 ProductEligibility>
data ProductEligibility = ProductEligibility
  { _productEligibilityDescription       :: Text -- ^ Description of the discount
  , _productEligibilityEligibilityType   :: ProductEligibilityType -- ^ The type of eligibility criteria described. See the note below for valid values and their meaning
-- WARNING
  , _productEligibilityAdditionalInfo    :: Maybe Text -- ^ Display text providing more information on the eligibility criteria. Mandatory if the eligibilityType field is set to OTHER
  , _productEligibilityAdditionalInfoUri :: Maybe URI -- ^ Link to a web page with more information on this eligibility criteria
  } deriving (Show, Eq)

productEligibilityDecoder :: Monad f => Decoder f ProductEligibility
productEligibilityDecoder =
  ProductEligibility
    <$> D.atKey "description" D.text
    <*> productEligibilityTypeDecoder
    <*> atKeyOptional' "additionalInfo" D.text
    <*> atKeyOptional' "additionalInfoUri" uriDecoder

instance JsonDecode OB ProductEligibility where
  mkDecoder = tagOb productEligibilityDecoder

productEligibilityEncoder :: Applicative f => Encoder f ProductEligibility
productEligibilityEncoder = E.mapLikeObj $ \p ->
  E.atKey' "description" E.text (_productEligibilityDescription p) .
  productEligibilityTypeFields (_productEligibilityEligibilityType p) .
  maybeOrAbsentE "additionalInfo" E.text (_productEligibilityAdditionalInfo p) .
  maybeOrAbsentE "additionalInfoUri" uriEncoder (_productEligibilityAdditionalInfoUri p)

instance JsonEncode OB ProductEligibility where
  mkEncoder = tagOb productEligibilityEncoder


-- | Description of the usage of the @eligibilityType@ field as it applies to products. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#producteligibilitytypedoc CDR AU v0.1.0 Product Eligibility Types>
data ProductEligibilityType =
    PEligibilityBusiness -- ^ "BUSINESS" Only business may apply for the account.
  | PEligibilityPensionRecipient -- ^ "PENSION_RECIPIENT" A recipient of a government pension may apply for the product.
  | PEligibilityMinAge Int -- ^ "MIN_AGE" Only customers older than a minimum age may apply. Use of @additionalValue@ field: The minimum age in years.
  | PEligibilityMaxAge Int -- ^ "MAX_AGE" Only customers younger than a maximum age may apply. Use of @additionalValue@ field: The minimum age in years.
  | PEligibilityMinIncome AmountString -- ^ "MIN_INCOME" The customer must have an income greater than a specified threshold to obtain the product. Use of @additionalValue@ field: Minimum income in AmountString format.
  | PEligibilityMinTurnover AmountString -- ^ "MIN_TURNOVER" Only a business with greater than a minimum turnover may apply. Use of @additionalValue@ field: Minimum turnover in AmountString format.
  | PEligibilityStaff -- ^ "STAFF" Only a staff member of the provider may apply.
  | PEligibilityStudent -- ^ "STUDENT" Only students may apply for the product.
  | PEligibilityEmploymentStatus Text -- ^ "EMPLOYMENT_STATUS" An eligibility constraint based on employment status applies. Use of @additionalValue@ field: A description of the status required.
  | PEligibilityResidencyStatus Text -- ^ "RESIDENCY_STATUS" An eligibility constraint based on residency status applies. Use of @additionalValue@ field: A description of the status required.
  | PEligibilityOther Text -- ^ "OTHER" Another eligibility criteria exists as described in the additionalInfo field (if this option is specified then the additionalInfo field is mandatory). Use of @additionalValue@ field: Value relevant to the criteria.
  deriving (Show, Eq)


productEligibilityTypeDecoder :: Monad f => Decoder f ProductEligibilityType
productEligibilityTypeDecoder = do
  eligibilityType <- D.atKey "eligibilityType" D.text
  additionalValue <- case eligibilityType of
    "BUSINESS" -> pure PEligibilityBusiness
    "PENSION_RECIPIENT" -> pure PEligibilityPensionRecipient
    "MIN_AGE" -> PEligibilityMinAge <$> (additionalValueDecoder D.int)
    "MAX_AGE" -> PEligibilityMaxAge <$> (additionalValueDecoder D.int)
    "MIN_INCOME" -> PEligibilityMinIncome <$> (additionalValueDecoder amountStringDecoder)
    "MIN_TURNOVER" -> PEligibilityMinTurnover <$> (additionalValueDecoder amountStringDecoder)
    "STAFF" -> pure PEligibilityStaff
    "STUDENT" -> pure PEligibilityStudent
    "EMPLOYMENT_STATUS" -> PEligibilityEmploymentStatus <$> (additionalValueDecoder D.text)
    "RESIDENCY_STATUS" -> PEligibilityResidencyStatus <$> (additionalValueDecoder D.text)
    "OTHER" -> PEligibilityOther <$> (additionalValueDecoder D.text)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

productEligibilityType'ToText :: ProductEligibilityType' -> Text
productEligibilityType'ToText = \case
  PEligibilityBusiness' -> "BUSINESS"
  PEligibilityPensionRecipient' -> "PENSION_RECIPIENT"
  PEligibilityMinAge' -> "MIN_AGE"
  PEligibilityMaxAge' -> "MAX_AGE"
  PEligibilityMinIncome' -> "MIN_INCOME"
  PEligibilityMinTurnover' -> "MIN_TURNOVER"
  PEligibilityStaff' -> "STAFF"
  PEligibilityStudent' -> "STUDENT"
  PEligibilityEmploymentStatus' -> "EMPLOYMENT_STATUS"
  PEligibilityResidencyStatus' -> "RESIDENCY_STATUS"
  PEligibilityOther' -> "OTHER"

data ProductEligibilityType' =
    PEligibilityBusiness'
  | PEligibilityPensionRecipient'
  | PEligibilityMinAge'
  | PEligibilityMaxAge'
  | PEligibilityMinIncome'
  | PEligibilityMinTurnover'
  | PEligibilityStaff'
  | PEligibilityStudent'
  | PEligibilityEmploymentStatus'
  | PEligibilityResidencyStatus'
  | PEligibilityOther'
  deriving (Eq, Show)

productEligibilityType'Encoder :: Applicative f => Encoder f ProductEligibilityType'
productEligibilityType'Encoder = flip contramap E.text productEligibilityType'ToText

productEligibilityTypeToType' :: ProductEligibilityType -> ProductEligibilityType'
productEligibilityTypeToType' (PEligibilityBusiness {})  = PEligibilityBusiness'
productEligibilityTypeToType' (PEligibilityPensionRecipient {}) = PEligibilityPensionRecipient'
productEligibilityTypeToType' (PEligibilityMinAge {}) = PEligibilityMinAge'
productEligibilityTypeToType' (PEligibilityMaxAge {})   = PEligibilityMaxAge'
productEligibilityTypeToType' (PEligibilityMinIncome {})  = PEligibilityMinIncome'
productEligibilityTypeToType' (PEligibilityMinTurnover {}) = PEligibilityMinTurnover'
productEligibilityTypeToType' (PEligibilityStaff {}) = PEligibilityStaff'
productEligibilityTypeToType' (PEligibilityStudent {})   = PEligibilityStudent'
productEligibilityTypeToType' (PEligibilityEmploymentStatus {}) = PEligibilityEmploymentStatus'
productEligibilityTypeToType' (PEligibilityResidencyStatus {}) = PEligibilityResidencyStatus'
productEligibilityTypeToType' (PEligibilityOther {})   = PEligibilityOther'

productEligibilityTypeFields :: (Monoid ws, Semigroup ws) => ProductEligibilityType -> MapLikeObj ws Json -> MapLikeObj ws Json
productEligibilityTypeFields pc =
  case pc of
    PEligibilityBusiness ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc)
    PEligibilityPensionRecipient ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc)
    PEligibilityMinAge v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" E.int v
    PEligibilityMaxAge v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" E.int v
    PEligibilityMinIncome v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    PEligibilityMinTurnover v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    PEligibilityStaff ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc)
    PEligibilityStudent ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc)
    PEligibilityEmploymentStatus v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PEligibilityResidencyStatus v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" E.text v
    PEligibilityOther v ->
      E.atKey' "eligibilityType" productEligibilityType'Encoder (productEligibilityTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB ProductEligibilityType where
  mkDecoder = tagOb productEligibilityTypeDecoder
