{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Products
  ( module Web.ConsumerData.Au.Api.Types.Banking.Products
  ) where

import Control.Error                       (note)
import Control.Lens
    (Getter, Prism', prism', to, ( # ), (^?))
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
_ProductEffective :: Prism' Text ProductEffective
_ProductEffective = prism' toT fromT
  where
    toT = \case
      ProductCurrent -> "CURRENT"
      ProductFuture  -> "FUTURE"
      ProductAll     -> "ALL"
    fromT = \case
      "CURRENT" -> Just ProductCurrent
      "FUTURE"  -> Just ProductFuture
      "ALL"     -> Just ProductAll
      _         -> Nothing


instance ToHttpApiData ProductEffective where
  toUrlPiece = (_ProductEffective #)

instance FromHttpApiData ProductEffective where
  parseUrlPiece t = note ("Invalid product effective query: " <> t) (t^?_ProductEffective)

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
