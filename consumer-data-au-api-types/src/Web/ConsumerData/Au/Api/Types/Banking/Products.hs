{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Products
  ( module Web.ConsumerData.Au.Api.Types.Banking.Products
  ) where

import Control.Lens                        (Getter, to)
import Data.Text                           (Text)
import GHC.Generics                        (Generic)
import Servant.API
    ((:>), Capture, FromHttpApiData, Get, QueryParam, ToHttpApiData,
    parseUrlPiece, toUrlPiece)
import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic                 ((:-))

import Web.ConsumerData.Au.Api.Types.Banking.Common.ProductDetail
import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
import Web.ConsumerData.Au.Api.Types.Response
import Web.ConsumerData.Au.Api.Types.Tag

newtype ProductId = ProductId { unProductId :: AsciiString } deriving (Eq, Show)
instance ToHttpApiData ProductId where
  toUrlPiece = toUrlPiece . unProductId
instance FromHttpApiData ProductId where
  parseUrlPiece = fmap ProductId . parseUrlPiece

data ProductEffective = ProductCurrent | ProductFuture | ProductAll deriving (Eq, Show)
instance ToHttpApiData ProductEffective where
  toUrlPiece ProductCurrent = "CURRENT"
  toUrlPiece ProductFuture  = "FUTURE"
  toUrlPiece ProductAll     = "ALL"

instance FromHttpApiData ProductEffective where
  parseUrlPiece "CURRENT" = Right ProductCurrent
  parseUrlPiece "FUTURE"  = Right ProductFuture
  parseUrlPiece "ALL"     = Right ProductAll
  parseUrlPiece t         = Left $ "Invalid product effective query: " <> t

type ProductsGetRoute r = r :-
  ( QueryParam "effective" ProductEffective
  :> QueryParam "updated-since" DateTimeString
  :> QueryParam "brand" Text
  :> QueryParam "product-category" ProductCategory
  :> PaginatedRoute (Get '[WaargJSON OB] ProductsGetResponse)
  )
type ProductsByIdGetRoute r = r :- Capture "productId" ProductId :> Get '[WaargJSON OB] ProductByIdResponse

data ProductsApi r = ProductsApi
  { _productsGet     :: ProductsGetRoute r
  , _productsByIdGet :: ProductsByIdGetRoute r
  } deriving (Generic)

productsGet :: Getter (ProductsApi r) (ProductsGetRoute r)
productsGet = to _productsGet

productsByIdGet :: Getter (ProductsApi r) (ProductsByIdGetRoute r)
productsByIdGet = to _productsByIdGet

type ProductsGetResponse = PaginatedResponse Products
type ProductByIdResponse = StandardResponse ProductDetail
