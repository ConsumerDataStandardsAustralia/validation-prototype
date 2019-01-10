{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Products where

import           Control.Lens             (Prism', prism, ( # ), (^?))
import           Data.Text                (Text)
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseQueryParam, toQueryParam)
import           Text.URI                 (URI)
import           Waargonaut.Decode        (Decoder)
import qualified Waargonaut.Decode        as D
import qualified Waargonaut.Decode.Error  as D
import           Waargonaut.Encode        (Encoder)
import qualified Waargonaut.Encode        as E
import           Waargonaut.Generic       (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject (MapLikeObj)
import           Waargonaut.Types.Json    (Json)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AsciiString, DateTimeString, asciiStringDecoder, asciiStringEncoder,
    dateTimeStringDecoder, dateTimeStringEncoder)
import Web.ConsumerData.Au.Api.Types.Response
    (uriDecoder, uriEncoder)
import Web.ConsumerData.Au.Api.Types.Tag


data Products = Products { getProducts :: [Product] }
  deriving (Eq, Show)

productsDecoder :: Monad f => Decoder f Products
productsDecoder =
  Products
    <$> D.atKey "products" (D.list productDecoder)

instance JsonDecode OB Products where
  mkDecoder = tagOb productsDecoder

productsEncoder :: Applicative f => Encoder f Products
productsEncoder = E.mapLikeObj $ \(Products ps) ->
  E.atKey' "products" (E.list productEncoder) ps

instance JsonEncode OB Products where
  mkEncoder = tagOb productsEncoder

-- | Product <https://consumerdatastandardsaustralia.github.io/standards/?swagger#tocBankingCommonSchemas CDR AU v0.1.0 Product>
data Product = Product
  { _productProductId             :: AsciiString -- ^ A provider specific unique identifier for this product. This identifier must be unique to a product but does not otherwise need to adhere to ID permanence guidelines.
  , _productEffectiveFrom         :: Maybe DateTimeString -- ^ A description of the product.
  , _productEffectiveTo           :: Maybe DateTimeString -- ^ The date and time at which this product will be retired and will no longer be offered.
  , _productLastUpdated           :: DateTimeString -- ^ A description of the product.
  , _productProductCategory       :: ProductCategory -- ^ The product category an account aligns withs.
  , _productName                  :: Text -- ^ The display name of the product.
  , _productDescription           :: Text -- ^ The description of the product.
  , _productBrand                 :: Text -- ^ A label of the brand for the product. Able to be used for filtering. For data providers with single brands this value is still required.
  , _productBrandName             :: Maybe Text -- ^ An optional display name of the brand
  , _productApplicationUri        :: Maybe URI -- ^ A link to an application web page where this product can be applied for.
  , _productIsNegotiable          :: Bool -- ^ Indicates whether the product is specifically designed so that fees and prices are negotiated depending on context. While all products are open to a degree of negotiation this flag indicates that negotiation is expected and thus that the provision of specific fees and rates is not applicable.
  , _productAdditionalInformation :: Maybe ProductAdditionalInformation -- ^ Object that contains links to additional information on specific topics.
  } deriving (Eq, Show)

productDecoder :: Monad f => Decoder f Product
productDecoder =
  Product
    <$> D.atKey "productId" asciiStringDecoder
    <*> atKeyOptional' "effectiveFrom" dateTimeStringDecoder
    <*> atKeyOptional' "effectiveTo" dateTimeStringDecoder
    <*> D.atKey "lastUpdated" dateTimeStringDecoder
    <*> D.atKey "productCategory" productCategoryDecoder
    <*> D.atKey "name" D.text
    <*> D.atKey "description" D.text
    <*> D.atKey "brand" D.text
    <*> atKeyOptional' "brandName" D.text
    <*> atKeyOptional' "applicationUri" uriDecoder
    <*> D.atKey "isNegotiable" D.bool
    <*> atKeyOptional' "additionalInformation" productAdditionalInformationDecoder

instance JsonDecode OB Product where
  mkDecoder = tagOb productDecoder

productEncoder :: Applicative f => Encoder f Product
productEncoder = E.mapLikeObj productFields

productFields
  :: (Monoid ws, Semigroup ws)
  => Product -> MapLikeObj ws Json -> MapLikeObj ws Json
productFields o =
  E.atKey' "productId" asciiStringEncoder (_productProductId o).
  maybeOrAbsentE "effectiveFrom" dateTimeStringEncoder (_productEffectiveFrom o).
  maybeOrAbsentE "effectiveTo" dateTimeStringEncoder (_productEffectiveTo o).
  E.atKey' "lastUpdated" dateTimeStringEncoder (_productLastUpdated o).
  E.atKey' "productCategory" productCategoryEncoder (_productProductCategory o).
  E.atKey' "name" E.text (_productName o).
  E.atKey' "description" E.text (_productDescription o).
  E.atKey' "brand" E.text (_productBrand o).
  maybeOrAbsentE "brandName" E.text (_productBrandName o).
  maybeOrAbsentE "applicationUri" uriEncoder (_productApplicationUri o).
  E.atKey' "isNegotiable" E.bool (_productIsNegotiable o).
  maybeOrAbsentE "additionalInformation" productAdditionalInformationEncoder (_productAdditionalInformation o)

instance JsonEncode OB Product where
  mkEncoder = tagOb productEncoder


data ProductAdditionalInformation = ProductAdditionalInformation
  { _paiOverviewUri       :: Maybe URI -- ^ General overview of the product.
  , _paiTermsUri          :: Maybe URI -- ^ Terms and conditions for the product.
  , _paiEligibilityUri    :: Maybe URI -- ^ Eligibility rules and criteria for the product.
  , _paiDeesAndPricingUri :: Maybe URI -- ^ Description of fees, pricing, discounts, exemptions and bonuses for the product.
  , _paiBundleUri         :: Maybe URI -- ^ Description of a bundle that this product can be part of.
  } deriving (Eq, Show)

productAdditionalInformationDecoder :: Monad f => Decoder f ProductAdditionalInformation
productAdditionalInformationDecoder =
  ProductAdditionalInformation
    <$> atKeyOptional' "overviewUri" uriDecoder
    <*> atKeyOptional' "termsUri" uriDecoder
    <*> atKeyOptional' "eligibilityUri" uriDecoder
    <*> atKeyOptional' "feesAndPricingUri" uriDecoder
    <*> atKeyOptional' "bundleUri" uriDecoder

instance JsonDecode OB ProductAdditionalInformation where
  mkDecoder = tagOb productAdditionalInformationDecoder

productAdditionalInformationEncoder :: Applicative f => Encoder f ProductAdditionalInformation
productAdditionalInformationEncoder = E.mapLikeObj $ \p ->
    maybeOrAbsentE "overviewUri" uriEncoder (_paiOverviewUri p) .
    maybeOrAbsentE "termsUri" uriEncoder (_paiTermsUri p) .
    maybeOrAbsentE "eligibilityUri" uriEncoder (_paiEligibilityUri p) .
    maybeOrAbsentE "feesAndPricingUri" uriEncoder (_paiDeesAndPricingUri p) .
    maybeOrAbsentE "bundleUri" uriEncoder (_paiBundleUri p)

instance JsonEncode OB ProductAdditionalInformation where
  mkEncoder = tagOb productAdditionalInformationEncoder


-- | The product category an account aligns withs. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductcategory CDR AU v0.1.0 ProductCategory>
data ProductCategory =
    PCPersAtCallDeposits -- ^ "PERS_AT_CALL_DEPOSITS"
  | PCBusAtCallDeposits -- ^ "BUS_AT_CALL_DEPOSITS"
  | PCTermDeposits -- ^ "TERM_DEPOSITS"
  | PCResidential_mortgages -- ^ "RESIDENTIAL_MORTGAGES"
  | PCPersCredAndChrgCards -- ^ "PERS_CRED_AND_CHRG_CARDS"
  | PCBusCredAndChrgCards -- ^ "BUS_CRED_AND_CHRG_CARDS"
  | PCPersLoans -- ^ "PERS_LOANS"
  | PCPersLeasing -- ^ "PERS_LEASING"
  | PCBusLeasing -- ^ "BUS_LEASING"
  | PCTradeFinance -- ^ "TRADE_FINANCE"
  | PCPersOverdraft -- ^ "PERS_OVERDRAFT"
  | PCBusOverdraft -- ^ "BUS_OVERDRAFT"
  | PCBusLoans -- ^ "BUS_LOANS"
  | PCForeignCurrAtCallDeposits -- ^ "FOREIGN_CURR_AT_CALL_DEPOSITS"
  | PCForeignCurrTermDeposits -- ^ "FOREIGN_CURR_TERM_DEPOSITS"
  | PCForeignCurrLoan -- ^ "FOREIGN_CURR_LOAN"
  | PCForeignCurrrenctOverdraft -- ^ "FOREIGN_CURRRENCT_OVERDRAFT"
  | PCTravelCard -- ^ "TRAVEL_CARD"
  deriving (Bounded, Enum, Eq, Ord, Show)

productCategoryText :: Prism' Text ProductCategory
productCategoryText =
  prism (\case
          PCPersAtCallDeposits -> "PERS_AT_CALL_DEPOSITS"
          PCBusAtCallDeposits -> "BUS_AT_CALL_DEPOSITS"
          PCTermDeposits -> "TERM_DEPOSITS"
          PCResidential_mortgages -> "RESIDENTIAL_MORTGAGES"
          PCPersCredAndChrgCards -> "PERS_CRED_AND_CHRG_CARDS"
          PCBusCredAndChrgCards -> "BUS_CRED_AND_CHRG_CARDS"
          PCPersLoans -> "PERS_LOANS"
          PCPersLeasing -> "PERS_LEASING"
          PCBusLeasing -> "BUS_LEASING"
          PCTradeFinance -> "TRADE_FINANCE"
          PCPersOverdraft -> "PERS_OVERDRAFT"
          PCBusOverdraft -> "BUS_OVERDRAFT"
          PCBusLoans -> "BUS_LOANS"
          PCForeignCurrAtCallDeposits -> "FOREIGN_CURR_AT_CALL_DEPOSITS"
          PCForeignCurrTermDeposits -> "FOREIGN_CURR_TERM_DEPOSITS"
          PCForeignCurrLoan -> "FOREIGN_CURR_LOAN"
          PCForeignCurrrenctOverdraft -> "FOREIGN_CURRRENCT_OVERDRAFT"
          PCTravelCard -> "TRAVEL_CARD"
      )
      (\case
          "PERS_AT_CALL_DEPOSITS" -> Right PCPersAtCallDeposits
          "BUS_AT_CALL_DEPOSITS" -> Right PCBusAtCallDeposits
          "TERM_DEPOSITS" -> Right PCTermDeposits
          "RESIDENTIAL_MORTGAGES" -> Right PCResidential_mortgages
          "PERS_CRED_AND_CHRG_CARDS" -> Right PCPersCredAndChrgCards
          "BUS_CRED_AND_CHRG_CARDS" -> Right PCBusCredAndChrgCards
          "PERS_LOANS" -> Right PCPersLoans
          "PERS_LEASING" -> Right PCPersLeasing
          "BUS_LEASING" -> Right PCBusLeasing
          "TRADE_FINANCE" -> Right PCTradeFinance
          "PERS_OVERDRAFT" -> Right PCPersOverdraft
          "BUS_OVERDRAFT" -> Right PCBusOverdraft
          "BUS_LOANS" -> Right PCBusLoans
          "FOREIGN_CURR_AT_CALL_DEPOSITS" -> Right PCForeignCurrAtCallDeposits
          "FOREIGN_CURR_TERM_DEPOSITS" -> Right PCForeignCurrTermDeposits
          "FOREIGN_CURR_LOAN" -> Right PCForeignCurrLoan
          "FOREIGN_CURRRENCT_OVERDRAFT" -> Right PCForeignCurrrenctOverdraft
          "TRAVEL_CARD" -> Right PCTravelCard
          t -> Left t
      )


instance ToHttpApiData ProductCategory where
  toQueryParam = (productCategoryText #)

instance FromHttpApiData ProductCategory where
  parseQueryParam t = maybe
    (Left $ "Not a valid product category: " <> t)
    Right
    (t^?productCategoryText)

productCategoryEncoder :: Applicative f => Encoder f ProductCategory
productCategoryEncoder = E.prismE productCategoryText E.text

productCategoryDecoder :: Monad f => Decoder f ProductCategory
productCategoryDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid ProductCategory")
  productCategoryText
  D.text
