{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Web.ConsumerData.Au.Api.Types.Banking.Products
  ( module Web.ConsumerData.Au.Api.Types.Banking.Products
  ) where

import Control.Lens                        (Getter, to)
import GHC.Generics                        (Generic)
import Servant.API
    ((:>), Capture, FromHttpApiData, Get, ToHttpApiData, parseUrlPiece,
    toUrlPiece)
import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic                 ((:-))

import Web.ConsumerData.Au.Api.Types.Banking.Common.ProductDetail
import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
import Web.ConsumerData.Au.Api.Types.Response
import Web.ConsumerData.Au.Api.Types.Tag

newtype ProductId = ProductId { unProductId :: Integer } deriving (Eq, Show)
instance ToHttpApiData ProductId where
  toUrlPiece = toUrlPiece . unProductId
instance FromHttpApiData ProductId where
  parseUrlPiece = fmap ProductId . parseUrlPiece

type ProductsGetRoute r = r :- Get '[WaargJSON OB] ProductsGetResponse
type ProductsByIdGetRoute r = r :- Capture "productId" ProductId :> Get '[WaargJSON OB] ProductByIdResponse

data ProductsApi r = ProductsApi
  { _productsGet     :: ProductsGetRoute r
  , _productsByIdGet :: ProductsByIdGetRoute r
  } deriving (Generic)

productsGet :: Getter (ProductsApi r) (ProductsGetRoute r)
productsGet = to _productsGet

productsByIdGet :: Getter (ProductsApi r) (ProductsByIdGetRoute r)
productsByIdGet = to _productsByIdGet

type ProductsGetResponse = PaginatedResponse [Product]
type ProductByIdResponse = StandardResponse ProductDetail
