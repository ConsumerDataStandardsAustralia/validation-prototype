{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Web.ConsumerData.Au.LambdaBank.Banking.ProductsTest where

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Control.Monad.IO.Class         (liftIO)
import Test.Tasty                     (TestTree)
import Test.Tasty.HUnit               (testCase, (@?=))
import Text.URI.QQ                    (uri)
import Web.ConsumerData.Au.Api.Client (apiClient, bankingClient, productsClient)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.WithServer

test_products :: [TestTree]
test_products =
  [ testCase "products test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . productsClient . productsGet
      liftIO $ res @?= Response testProducts
        (LinksPaginated
         [uri|http://localhost:1337/banking/products|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "productsById test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . productsClient . productsByIdGet . to ($ ProductId (AsciiString "product-id-5"))
      liftIO $ res @?= Response testProductDetail
        (LinksStandard [uri|http://localhost:1337/banking/products/product-id-5|])
        MetaStandard
  ]
