{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.Common.CustomerTest where

import Control.Lens

import Control.Monad.IO.Class   (liftIO)
import Text.URI.QQ              (uri)
import Test.Tasty               (TestTree)
import Test.Tasty.HUnit         (testCase, (@?=))
import WaargoRoundTrip          (roundTripTest)

import Web.ConsumerData.Au.Api.Client
import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.FakeServer


test_customer :: [TestTree]
test_customer =
  [ testCase "briefGet" . withServer 1337 $ do
      res <- apiClient ^. commonClient . customerClient . customerBriefGet
      liftIO $ res @?= (Response
--        (CustomerPerson testPerson)
        (CustomerOrganisation testOrganisation)
        (LinksStandard [uri|http://localhost:1337/common/customer|])
        MetaStandard
        )

  , testCase "detailGet" . withServer 1337 $ do
      res <- apiClient ^. commonClient . customerClient . customerDetailsGet
      liftIO $ res @?= (Response
        (CustomerDetailPerson testPersonDetail)
--        (CustomerDetailOrganisation testOrganisationDetail)
        (LinksStandard [uri|http://localhost:1337/common/customer/detail|])
        MetaStandard
        )
  , roundTripTest
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
