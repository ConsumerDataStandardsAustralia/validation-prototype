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
  { _payeesGet = getPayeesAll >>= \ps -> bankPaginatedResponse
    ps
    (fakePaginator Nothing (const $ links^.bankingLinks.bankingPayeesLinks.payeesGet))
  , _payeesByIdGet = \payeeId -> getPayeeDetail payeeId >>= \pd -> bankStandardResponse
    pd
    (links^.bankingLinks.bankingPayeesLinks.payeesByIdGet $ payeeId)
  }
