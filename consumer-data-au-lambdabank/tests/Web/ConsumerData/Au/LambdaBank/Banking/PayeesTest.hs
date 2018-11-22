{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Web.ConsumerData.Au.LambdaBank.Banking.PayeesTest where

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Control.Monad.IO.Class         (liftIO)
import Test.Tasty                     (TestTree)
import Test.Tasty.HUnit               (testCase, (@?=))
import Text.URI.QQ                    (uri)
import Web.ConsumerData.Au.Api.Client (apiClient, bankingClient, payeesClient)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.WithServer

test_payees :: [TestTree]
test_payees =
  [ testCase "payees test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . payeesClient . payeesGet
      liftIO $ res @?= Response testPayees
        (LinksPaginated
         [uri|http://localhost:1337/banking/payees|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "payeesById test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . payeesClient . payeesByIdGet . to ($ PayeeId "5")
      liftIO $ res @?= Response testPayeeDetail
        (LinksStandard [uri|http://localhost:1337/banking/payees/5|])
        MetaStandard
  ]
