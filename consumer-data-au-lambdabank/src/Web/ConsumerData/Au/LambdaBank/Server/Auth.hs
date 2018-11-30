module Web.ConsumerData.Au.LambdaBank.Server.Auth where

import Web.ConsumerData.Au.Api.Types (AuthApi)

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.Server.Banking.Accounts (accountsServer)
import Web.ConsumerData.Au.LambdaBank.Server.Banking.Payees   (payeesServer)
import Web.ConsumerData.Au.LambdaBank.Server.Banking.Products (productsServer)
import Web.ConsumerData.Au.LambdaBank.Server.Internal         (LambdaBankM)

authServer :: ToServant AuthApi (AsServerT LambdaBankM)
authServer = genericServerT AuthApi
  { authorise = authoriseServer }
