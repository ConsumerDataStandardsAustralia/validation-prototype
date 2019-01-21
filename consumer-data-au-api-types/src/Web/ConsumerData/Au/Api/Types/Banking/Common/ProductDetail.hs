{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.ProductDetail where

import           Control.Lens               (Prism', prism, ( # ))
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Text.URI                   (URI)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag

import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
    (Product, productDecoder, productFields)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Constraint
    (ProductConstraints, productConstraintsDecoder, productConstraintsEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.DepositRate
    (ProductDepositRates, productDepositRatesDecoder,
    productDepositRatesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Eligibility
    (ProductEligibilities, productEligibilitiesDecoder,
    productEligibilitiesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Feature
    (ProductFeatures, productFeaturesDecoder, productFeaturesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Fee
    (ProductFees, productFeesDecoder, productFeesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.LendingRate
    (ProductLendingRates, productLendingRatesDecoder,
    productLendingRatesEncoder)


data ProductDetail = ProductDetail
  { _productDetailProduct       :: Maybe Product
  , _productDetailBundles       :: Maybe ProductBundles
  , _productDetailFeatures      :: Maybe ProductFeatures
  , _productDetailConstraints   :: Maybe ProductConstraints
  , _productDetailEligibility   :: Maybe ProductEligibilities
  , _productDetailFees          :: Maybe ProductFees
  , _productDetailDepositRates  :: Maybe ProductDepositRates
  , _productDetailLendingRates  :: Maybe ProductLendingRates
  , _productDetailRepaymentType :: Maybe ProductRepaymentType
  } deriving (Eq, Show)

productDetailDecoder :: Monad f => Decoder f ProductDetail
productDetailDecoder =
  ProductDetail
    <$> (D.maybeOrNull productDecoder)
    <*> atKeyOptional' "bundles" productBundlesDecoder
    <*> atKeyOptional' "features" productFeaturesDecoder
    <*> atKeyOptional' "constraints" productConstraintsDecoder
    <*> atKeyOptional' "eligibility" productEligibilitiesDecoder
    <*> atKeyOptional' "fees" productFeesDecoder
    <*> atKeyOptional' "depositRates" productDepositRatesDecoder
    <*> atKeyOptional' "lendingRates" productLendingRatesDecoder
    <*> atKeyOptional' "repaymentType" productRepaymentTypeDecoder

instance JsonDecode OB ProductDetail where
  mkDecoder = tagOb productDetailDecoder

instance JsonEncode OB ProductDetail where
  mkEncoder = tagOb productDetailEncoder

productDetailEncoder :: Applicative f => Encoder f ProductDetail
productDetailEncoder = E.mapLikeObj $ \pd ->
  maybe id productFields (_productDetailProduct pd) .
  maybeOrAbsentE "bundles" productBundlesEncoder (_productDetailBundles pd) .
  maybeOrAbsentE "features" productFeaturesEncoder (_productDetailFeatures pd) .
  maybeOrAbsentE "constraints" productConstraintsEncoder (_productDetailConstraints pd) .
  maybeOrAbsentE "eligibility" productEligibilitiesEncoder (_productDetailEligibility pd) .
  maybeOrAbsentE "fees" productFeesEncoder (_productDetailFees pd) .
  maybeOrAbsentE "depositRates" productDepositRatesEncoder (_productDetailDepositRates pd) .
  maybeOrAbsentE "lendingRates" productLendingRatesEncoder (_productDetailLendingRates pd) .
  maybeOrAbsentE "repaymentType" productRepaymentTypeEncoder (_productDetailRepaymentType pd)


newtype ProductBundles =
  ProductBundles { getProductBundles :: [ProductBundle] }
  deriving (Eq, Show)

productBundlesDecoder :: Monad f => Decoder f ProductBundles
productBundlesDecoder = ProductBundles <$> D.list productBundleDecoder

productBundlesEncoder :: Applicative f => Encoder f ProductBundles
productBundlesEncoder = getProductBundles >$< E.list productBundleEncoder

instance JsonDecode OB ProductBundles where
  mkDecoder = tagOb productBundlesDecoder

instance JsonEncode OB ProductBundles where
  mkEncoder = tagOb productBundlesEncoder


data ProductBundle = ProductBundle
  { _productBundleName              :: Text
  , _productBundleDescription       :: Text
  , _productBundleAdditionalInfoUri :: Maybe URI
  , _productBundleProductIds        :: [Text]
  } deriving (Eq, Show)

productBundleDecoder :: Monad f => Decoder f ProductBundle
productBundleDecoder =
  ProductBundle
    <$> D.atKey "name" D.text
    <*> D.atKey "description" D.text
    <*> atKeyOptional' "applicationUri" uriDecoder
    <*> D.atKey "productIds" (D.list D.text)

instance JsonDecode OB ProductBundle where
  mkDecoder = tagOb productBundleDecoder

productBundleEncoder :: Applicative f => Encoder f ProductBundle
productBundleEncoder = E.mapLikeObj $ \p ->
  E.atKey' "name" E.text (_productBundleName p) .
  E.atKey' "description" E.text (_productBundleDescription p) .
  maybeOrAbsentE "applicationUri" uriEncoder (_productBundleAdditionalInfoUri p) .
  E.atKey' "productIds" (E.list E.text) (_productBundleProductIds p)

instance JsonEncode OB ProductBundle where
  mkEncoder = tagOb productBundleEncoder


data ProductRepaymentType =
    PRepaymentTypeInterestOnly -- ^ "INTEREST_ONLY"
  | PRepaymentTypePrincipalAndInterest -- ^ "PRINCIPAL_AND_INTEREST"
  | PRepaymentTypeNegotiable -- ^ "NEGOTIABLE"
  deriving (Bounded, Enum, Eq, Ord, Show)

productRepaymentTypeText ::
  Prism' Text ProductRepaymentType
productRepaymentTypeText =
  prism (\case
          PRepaymentTypeInterestOnly -> "INTEREST_ONLY"
          PRepaymentTypePrincipalAndInterest -> "PRINCIPAL_AND_INTEREST"
          PRepaymentTypeNegotiable -> "NEGOTIABLE"
      )
      (\case
          "INTEREST_ONLY" -> Right PRepaymentTypeInterestOnly
          "PRINCIPAL_AND_INTEREST" -> Right PRepaymentTypePrincipalAndInterest
          "NEGOTIABLE"-> Right PRepaymentTypeNegotiable
          t -> Left t
      )

productRepaymentTypeDecoder :: Monad m => Decoder m  ProductRepaymentType
productRepaymentTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid product repayment type")
  productRepaymentTypeText
  D.text

productRepaymentTypeEncoder :: Applicative f => Encoder f ProductRepaymentType
productRepaymentTypeEncoder =
  E.prismE productRepaymentTypeText E.text
