{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Banking.Accounts where

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic    (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.FakeData        (fakePaginator)
import Web.ConsumerData.Au.LambdaBank.Model
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankPaginatedResponse, bankStandardResponse)

accountsServer :: ToServant AccountsApi (AsServerT LambdaBankM)
accountsServer = genericServerT AccountsApi
    { _accountsGet = \status owned prodCat pn ps -> do
        accts <- getAccounts
        bankPaginatedResponse accts
          (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsGet. to (\f -> f status owned prodCat)))
    , _accountsBalancesGet = \status owned prodCat pn ps -> do
        balances <- getBalancesAll
        bankPaginatedResponse balances
          (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet.to (\f -> f status owned prodCat)))
    , _accountsBalancesPost = \request -> getBalancesForAccounts (_requestData request) >>= \bs -> bankStandardResponse
      bs
      (links^.bankingLinks.bankingAccountsLinks.accountsBalancesPost)
    , _accountsTransactionsGet = \st et minA maxA xactT status owned prodCat pn ps -> do
        xacts <- getTransactionsAll
        bankPaginatedResponse xacts
          (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsTransactionsGet.to (\f -> f st et minA maxA xactT status owned prodCat)))
    , _accountsTransactionsPost = \st et minA maxA xactT request pn ps -> do
        xacts <- getTransactionsForAccounts (_requestData request)
        bankPaginatedResponse xacts
          (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsTransactionsPost. to (\f -> f st et minA maxA xactT)))
    , _accountsDirectDebitsGet = \owned prodCat pn ps -> do
        debits <- getDirectDebitsAll
        bankPaginatedResponse debits
          (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsGet. to (\f -> f owned prodCat)))
    , _accountsDirectDebitsPost = \request pn ps -> do
        xacts <- getDirectDebitsForAccounts (_requestData request)
        bankPaginatedResponse xacts
          (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsPost))
    , _accountsById = \accountId -> genericServerT AccountApi
      { _accountGet = getAccountById accountId >>= \ad -> bankStandardResponse
        ad
        (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountGet)
      , _accountTransactionsGet = \st et minA maxA xactT pn ps -> do
          xacts <- getTransactionsForAccount accountId
          bankPaginatedResponse xacts
            (fakePaginator pn ps (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionsGet.to (\f -> f st et minA maxA xactT)))
      , _accountTransactionByIdGet = \transactionId ->
          getTransactionDetailForAccountTransaction accountId transactionId >>= \xacts -> bankStandardResponse
            xacts
            ((links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionByIdGet) transactionId)
      , _accountDirectDebitsGet = \pn ps -> do
          debits <- getDirectDebitsForAccount accountId
          bankPaginatedResponse debits
            (fakePaginator pn ps
             (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountDirectDebitsGet))
      }
    }
