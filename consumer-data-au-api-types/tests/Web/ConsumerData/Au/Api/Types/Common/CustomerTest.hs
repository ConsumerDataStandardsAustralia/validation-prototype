{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
module Web.ConsumerData.Au.Api.Types.Common.CustomerTest where

import Test.Tasty      (TestTree)
import WaargoRoundTrip (roundTripTest)

import Web.ConsumerData.Au.Api.Types

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
