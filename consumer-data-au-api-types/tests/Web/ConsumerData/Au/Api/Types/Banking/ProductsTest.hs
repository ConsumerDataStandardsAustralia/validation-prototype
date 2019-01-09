{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Web.ConsumerData.Au.Api.Types.Banking.ProductsTest where

import Control.Lens
import Data.Functor.Identity (Identity)
import Data.Tagged           (Tagged)
import Data.Time             (UTCTime (UTCTime), fromGregorian)
import Test.Tasty            (TestTree)
import Text.URI.QQ           (uri)
import Waargonaut.Decode     (Decoder)
import Waargonaut.Encode     (Encoder)
import Waargonaut.Generic    (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip       (roundTripTest)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.LinkTestHelpers
    (linkTest, paginatedLinkTest)
import Web.ConsumerData.Au.Api.Types.PrismTestHelpers (testEnumPrismTripping)
import Web.ConsumerData.Au.Api.Types.Tag

test_productsLinks :: [TestTree]
test_productsLinks =
  [ paginatedLinkTest "Get Products no params"
    ((links^.bankingLinks.bankingProductsLinks.productsGet) Nothing Nothing Nothing Nothing)
    [uri|http://localhost/banking/products|]
  , paginatedLinkTest "Get Products all params"
    ((links^.bankingLinks.bankingProductsLinks.productsGet)
     (Just ProductFuture)
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 4) 2929))
     (Just "awesome product")
     (Just PCTravelCard)
    ) [uri|http://localhost/banking/products?effective=FUTURE&updated-since=2019-01-04T00:48:49Z&brand=awesome+product&product-category=TRAVEL_CARD|]
  , linkTest "Get Product Detail"
    (links^.bankingLinks.bankingProductsLinks.productsByIdGet.to ($ ProductId (AsciiString "123"))) [uri|http://localhost/banking/products/123|]
  ]

test_roundTripEnum :: [TestTree]
test_roundTripEnum =
  [ testEnumPrismTripping "EnumProductCategory" _EnumProductCategory
  ]

test_roundTripProducts :: TestTree
test_roundTripProducts = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity (PaginatedResponse Products)))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity (PaginatedResponse Products)))))
  "ProductsRoundTrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/ProductsTest/products.golden.json"



test_roundTripProductDetail :: TestTree
test_roundTripProductDetail = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity (StandardResponse ProductDetail)))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity (StandardResponse ProductDetail)))))
  "ProductDetailRoundTrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/ProductsTest/productdetail.golden.json"
