{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Common.Customer where

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.Model
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankStandardResponse)

customerServer :: ToServant CustomerApi (AsServerT LambdaBankM)
customerServer = genericServerT CustomerApi
  { _customerBriefGet = getCustomer >>= \c -> bankStandardResponse
    c
    (links ^.commonLinks.customerLinks.customerBriefGet)
  , _customerDetailsGet = getCustomerDetail >>= \c -> bankStandardResponse
    c
    --(CustomerDetailOrganisation testOrganisationDetail)
    (links ^.commonLinks.customerLinks.customerDetailsGet)
  }
