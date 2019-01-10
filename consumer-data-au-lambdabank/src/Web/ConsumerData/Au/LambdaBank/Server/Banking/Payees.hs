{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Banking.Payees where

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.FakeData                (fakePaginator)
import Web.ConsumerData.Au.LambdaBank.Model
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankPaginatedResponse, bankStandardResponse)

payeesServer :: ToServant PayeesApi (AsServerT LambdaBankM)
payeesServer =genericServerT PayeesApi
  { _payeesGet = \pt pn ps -> do
    payees <- getPayeesAll pt pn ps
    bankPaginatedResponse payees (fakePaginator pn ps (links^.bankingLinks.bankingPayeesLinks.payeesGet. to ($ pt)))
  , _payeesByIdGet = \payeeId -> do
      payee <- getPayeeDetail payeeId
      bankStandardResponse payee (links^.bankingLinks.bankingPayeesLinks.payeesByIdGet $ payeeId)
  }
