{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
module Web.ConsumerData.Au.Api.Types.Common.CustomerTest where

import Control.Lens

import Test.Tasty      (TestTree)
import Text.URI.QQ     (uri)
import WaargoRoundTrip (roundTripTest)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.LinkTestHelpers (linkTest)

test_customerLinks :: [TestTree]
test_customerLinks =
  [ linkTest "Get Customer"
    (links^.commonLinks.customerLinks.customerBriefGet) [uri|http://localhost/common/customer|]
  , linkTest "Get Customer Detail"
    (links^.commonLinks.customerLinks.customerDetailsGet) [uri|http://localhost/common/customer/detail|]
  ]

test_customer :: [TestTree]
test_customer =
  [ roundTripTest
    customerResponseDecoder
    customerResponseEncoder
    "CustomerPersonRoundTrip"
    "tests/Web/ConsumerData/Au/Api/Types/Common/CustomerTest/person.json"
  , roundTripTest
    customerDetailResponseDecoder
    customerDetailResponseEncoder
    "CustomerDetailsPersonRoundTrip"
    "tests/Web/ConsumerData/Au/Api/Types/Common/CustomerTest/personDetail.json"
  , roundTripTest
    customerResponseDecoder
    customerResponseEncoder
    "CustomerOrganisationRoundTrip"
    "tests/Web/ConsumerData/Au/Api/Types/Common/CustomerTest/organisation.json"
  , roundTripTest
    customerDetailResponseDecoder
    customerDetailResponseEncoder
    "CustomerDetailsOrganisationRoundTrip"
    "tests/Web/ConsumerData/Au/Api/Types/Common/CustomerTest/organisationDetail.json"
  ]
