{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer
  , module CustomerResponse
  , module CustomerDetailResponse
  ) where

import           Control.Lens                        (Getter, to)
import           GHC.Generics                        (Generic)
import           Servant.API                         ((:>), Get)
import           Servant.API.ContentTypes.Waargonaut (WaargJSON)
import           Servant.API.Generic                 ((:-))

import Web.ConsumerData.Au.Api.Types.Response
import Web.ConsumerData.Au.Api.Types.Tag
import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.CustomerDetailResponse as CustomerDetailResponse
import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.CustomerResponse as CustomerResponse

type CustomerBriefGetRoute r = r :- Get '[WaargJSON OB] (StandardResponse CustomerResponse)
type CustomerDetailsGetRoute r = r :- "detail" :> Get '[WaargJSON OB] (StandardResponse CustomerDetailResponse)

data CustomerApi r = CustomerApi
  { _customerBriefGet   :: CustomerBriefGetRoute r
  , _customerDetailsGet :: CustomerDetailsGetRoute r
  } deriving (Generic)

customerBriefGet :: Getter (CustomerApi r) (CustomerBriefGetRoute r)
customerBriefGet = to _customerBriefGet

customerDetailsGet :: Getter (CustomerApi r) (CustomerDetailsGetRoute r)
customerDetailsGet = to _customerDetailsGet
