{-# LANGUAGE TypeApplications #-}

module Web.ConsumerData.Au.Api.Types.Banking.ProductsTest where

import           Data.Functor.Identity                                 (Identity)
import           Data.Tagged                                           (Tagged)
import           Test.Tasty                                            (TestTree)
import           Waargonaut.Decode                                     (Decoder)
import           Waargonaut.Encode                                     (Encoder)
import           Waargonaut.Generic                                    (mkDecoder,
                                                                        mkEncoder,
                                                                        untag)
import           WaargoRoundTrip                                       (roundTripTest)

import           Web.ConsumerData.Au.Api.Types
import           Web.ConsumerData.Au.Api.Types.Tag


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
