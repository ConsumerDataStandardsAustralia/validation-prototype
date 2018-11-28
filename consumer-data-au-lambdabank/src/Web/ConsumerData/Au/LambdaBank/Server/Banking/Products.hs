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
  { _productsGet = getProductsAll >>= \ps -> bankPaginatedResponse
    ps
    (fakePaginator Nothing (const $ links^.bankingLinks.bankingProductsLinks.productsGet))
  , _productsByIdGet = \productId -> getProductDetail productId >>= \pd -> bankStandardResponse
    pd
    (links^.bankingLinks.bankingProductsLinks.productsByIdGet $ productId)
  }
