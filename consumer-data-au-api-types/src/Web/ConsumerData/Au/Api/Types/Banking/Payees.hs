{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Payees
  ( module Web.ConsumerData.Au.Api.Types.Banking.Payees
  ) where

import Control.Error                       (note)
import Control.Lens
    (Getter, Prism', prism', to, ( # ), (^?))
import Data.Text                           (Text)
import GHC.Generics                        (Generic)
import Servant.API
    ((:>), Capture, FromHttpApiData, Get, QueryParam, ToHttpApiData,
    parseQueryParam, toQueryParam)
import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic                 ((:-))

import Web.ConsumerData.Au.Api.Types.Banking.Common.PayeeDetail
import Web.ConsumerData.Au.Api.Types.Banking.Common.Payees
import Web.ConsumerData.Au.Api.Types.Response
import Web.ConsumerData.Au.Api.Types.Tag


data PayeeTypeParam =
    PayeeTypeParamDomestic
  | PayeeTypeParamInternational
  | PayeeTypeParamBiller
  | PayeeTypeParamAll
  deriving (Eq, Show, Enum, Bounded)

_PayeeTypeParam :: Prism' Text PayeeTypeParam
_PayeeTypeParam = prism' toT fromT
  where
    toT = \case
      PayeeTypeParamDomestic -> "DOMESTIC"
      PayeeTypeParamInternational -> "INTERNATIONAL"
      PayeeTypeParamBiller -> "BILLER"
      PayeeTypeParamAll -> "ALL"
    fromT = \case
      "DOMESTIC"   -> Just PayeeTypeParamDomestic
      "INTERNATIONAL" -> Just PayeeTypeParamInternational
      "BILLER" -> Just PayeeTypeParamBiller
      "ALL"    -> Just PayeeTypeParamAll
      _        -> Nothing

instance ToHttpApiData PayeeTypeParam where
  toQueryParam = (_PayeeTypeParam #)

instance FromHttpApiData PayeeTypeParam where
  parseQueryParam t = note ("Invalid PayeeTypeParam: " <> t) (t^?_PayeeTypeParam)


type PayeesGetRoute r = r :- QueryParam "type" PayeeTypeParam :> PaginatedRoute (Get '[WaargJSON OB] PayeesResponse)
type PayeesByIdGetRoute r = r :- Capture "payeeId" PayeeId :> Get '[WaargJSON OB] PayeeDetailResponse

data PayeesApi r = PayeesApi
  { _payeesGet     :: PayeesGetRoute r
  , _payeesByIdGet :: PayeesByIdGetRoute r
  } deriving (Generic)

payeesGet :: Getter (PayeesApi r) (PayeesGetRoute r)
payeesGet = to _payeesGet

payeesByIdGet :: Getter (PayeesApi r) (PayeesByIdGetRoute r)
payeesByIdGet = to _payeesByIdGet

type PayeesResponse = PaginatedResponse Payees
type PayeeDetailResponse = StandardResponse PayeeDetail
