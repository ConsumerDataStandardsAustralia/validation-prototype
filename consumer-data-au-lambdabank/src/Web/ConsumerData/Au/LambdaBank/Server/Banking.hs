{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Banking where

{--
Notes: this is just moving the FakeServer from the types tests across for now.
We will split it apart and make it into a real server with config and a database
and not all in one file. Don't be too upset
that it looks heinous for now!
--}

import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.Server.Banking.Accounts (accountsServer)
import Web.ConsumerData.Au.LambdaBank.Server.Banking.Payees   (payeesServer)
import Web.ConsumerData.Au.LambdaBank.Server.Banking.Products (productsServer)
import Web.ConsumerData.Au.LambdaBank.Server.Internal         (LambdaBankM)

bankingServer :: ToServant BankingApi (AsServerT LambdaBankM)
bankingServer = genericServerT BankingApi
  { _bankingAccounts = accountsServer
  , _bankingPayees = payeesServer
  , _bankingProducts = productsServer
  }
