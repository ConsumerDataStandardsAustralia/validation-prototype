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


data Product = Product
  { _productProductId             :: AsciiString
  , _productEffectiveFrom         :: Maybe DateTimeString
  , _productEffectiveTo           :: Maybe DateTimeString
  , _productLastUpdated           :: DateTimeString
  , _productProductCategory       :: EnumProductCategory
  , _productName                  :: Text
  , _productDescription           :: Text
  , _productBrand                 :: Text
  , _productBrandName             :: Maybe Text
  , _productApplicationUri        :: Maybe URI
  , _productIsNegotiable          :: Bool
  , _productAdditionalInformation :: Maybe ProductAdditionalInformation
  } deriving (Eq, Show)

productDecoder :: Monad f => Decoder f Product
productDecoder =
  Product
    <$> D.atKey "productId" asciiStringDecoder
    <*> atKeyOptional' "effectiveFrom" dateTimeStringDecoder
    <*> atKeyOptional' "effectiveTo" dateTimeStringDecoder
    <*> D.atKey "lastUpdated" dateTimeStringDecoder
    <*> D.atKey "productCategory" enumProductCategoryDecoder
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
  E.atKey' "productCategory" enumProductCategoryEncoder (_productProductCategory o).
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
  { _paiOverviewUri       :: Maybe URI
  , _paiTermsUri          :: Maybe URI
  , _paiEligibilityUri    :: Maybe URI
  , _paiDeesAndPricingUri :: Maybe URI
  , _paiBundleUri         :: Maybe URI
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


data EnumProductCategory =
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

_EnumProductCategory :: Prism' Text EnumProductCategory
_EnumProductCategory =
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


instance ToHttpApiData EnumProductCategory where
  toQueryParam = (_EnumProductCategory #)

instance FromHttpApiData EnumProductCategory where
  parseQueryParam t = maybe
    (Left $ "Not a valid product category: " <> t)
    Right
    (t^?_EnumProductCategory)

enumProductCategoryEncoder :: Applicative f => Encoder f EnumProductCategory
enumProductCategoryEncoder = E.prismE _EnumProductCategory E.text

enumProductCategoryDecoder :: Monad f => Decoder f EnumProductCategory
enumProductCategoryDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid ProductCategory")
  _EnumProductCategory
  D.text
