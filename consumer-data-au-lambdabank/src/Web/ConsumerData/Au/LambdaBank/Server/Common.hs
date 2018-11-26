{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Common where

{--
Notes: this is just moving the FakeServer from the types tests across for now.
We will split it apart and make it into a real server with config and a database
and not all in one file. Don't be too upset
that it looks heinous for now!
--}

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankStandardResponse)

commonServer :: ToServant CustomerApi (AsServerT LambdaBankM)
commonServer = genericServerT CommonApi
  { _customer = genericServerT CustomerApi
    { _customerBriefGet = bankStandardResponse
      --(CustomerPerson testPerson)
      (CustomerOrganisation testOrganisation)
      (links ^.commonLinks.customerLinks.customerBriefGet)
    , _customerDetailsGet = bankStandardResponse
      (CustomerDetailPerson testPersonDetail)
      --(CustomerDetailOrganisation testOrganisationDetail)
      (links ^.commonLinks.customerLinks.customerDetailsGet)
    }
  }
