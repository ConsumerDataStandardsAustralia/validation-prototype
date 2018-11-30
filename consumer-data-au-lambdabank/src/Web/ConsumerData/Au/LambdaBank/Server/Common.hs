{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Common where

import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.Server.Internal (LambdaBankM)
import Web.ConsumerData.Au.LambdaBank.Server.Common.Customer (customerServer)

commonServer :: ToServant CommonApi (AsServerT LambdaBankM)
commonServer = genericServerT CommonApi
  { _customer = customerServer
  }
