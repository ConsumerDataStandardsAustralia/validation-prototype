{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.LambdaBank.Common.CustomerTest where

import Control.Lens
import Web.ConsumerData.Au.Api.Client
import Web.ConsumerData.Au.Api.Types

import Control.Monad.IO.Class (liftIO)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       (testCase, (@?=))
import Text.URI.QQ            (uri)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.WithServer

test_customer :: [TestTree]
test_customer =
  [ testCase "briefGet" . withServer 1337 $ do
      res <- apiClient ^. commonClient . customerClient . customerBriefGet
      liftIO $ res @?= Response
        (CustomerPerson testPerson)
--        (CustomerOrganisation testOrganisation)
        (LinksStandard [uri|http://localhost:1337/common/customer|])
        MetaStandard

  , testCase "detailGet" . withServer 1337 $ do
      res <- apiClient ^. commonClient . customerClient . customerDetailsGet
      liftIO $ res @?= Response
        (CustomerDetailPerson testPersonDetail)
--        (CustomerDetailOrganisation testOrganisationDetail)
        (LinksStandard [uri|http://localhost:1337/common/customer/detail|])
        MetaStandard
  ]
