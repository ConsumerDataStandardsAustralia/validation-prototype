{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Banking.Products where

import Web.ConsumerData.Au.Api.Types

import Control.Lens

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.FakeData                (fakePaginator)
import Web.ConsumerData.Au.LambdaBank.Model
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankPaginatedResponse, bankStandardResponse)

productsServer :: ToServant ProductsApi (AsServerT LambdaBankM)
productsServer = genericServerT ProductsApi
  { _productsGet = \pe dts t pc pn ps -> do
      products <- getProductsAll pe dts t pc pn ps
      bankPaginatedResponse products (fakePaginator pn ps (links^.bankingLinks.bankingProductsLinks.productsGet.to (\f -> f pe dts t pc)))
  , _productsByIdGet = \productId -> do
      pd <- getProductDetail productId
      bankStandardResponse pd (links^.bankingLinks.bankingProductsLinks.productsByIdGet $ productId)
  }
