{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Web.ConsumerData.Au.Api.Types.Banking.Payees
  ( module Web.ConsumerData.Au.Api.Types.Banking.Payees
  ) where

import Control.Lens        (Getter, to)
import GHC.Generics        (Generic)
import Servant.API
    ((:>), Capture, Get, QueryParam)
import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic ((:-))

import Web.ConsumerData.Au.Api.Types.Banking.Common.PayeeDetail
import Web.ConsumerData.Au.Api.Types.Banking.Common.Payees
import Web.ConsumerData.Au.Api.Types.Response
import Web.ConsumerData.Au.Api.Types.Tag

type PayeesGetRoute r = r :- QueryParam "type" PayeeType :> PaginatedRoute (Get '[WaargJSON OB] PayeesResponse)
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
