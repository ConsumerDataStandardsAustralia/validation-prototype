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
    { _accountsGet = \pMay -> getAccounts >>= \as -> bankPaginatedResponse
      as
      (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsGet))
    , _accountsBalancesGet = \pMay -> getBalancesAll >>= \bs -> bankPaginatedResponse
      bs
      (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
    , _accountsBalancesPost = getBalancesForAccounts (error "TODO") >>= \bs -> bankStandardResponse
      bs
      (links^.bankingLinks.bankingAccountsLinks.accountsBalancesPost)
    , _accountsTransactionsGet = \pMay -> getTransactionsAll >>= \as -> bankPaginatedResponse
      as
      (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsTransactionsGet))
    , _accountsTransactionsPost = getTransactionsForAccounts (error "TODO") >>= \as -> bankPaginatedResponse
      as
      (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsTransactionsPost))
    , _accountsDirectDebitsGet = \pMay -> getDirectDebitsAll >>= \dds -> bankPaginatedResponse
      dds
      (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsGet))
    , _accountsDirectDebitsPost = getDirectDebitsForAccounts (error "TODO") >>= \dds -> bankPaginatedResponse
      dds
      (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsPost))
    , _accountsById = \accountId -> genericServerT AccountApi
      { _accountGet                = getAccountById accountId >>= \ad -> bankStandardResponse
        ad
        (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountGet)
      , _accountTransactionsGet    = getTransactionsForAccount accountId >>= \xacts -> bankPaginatedResponse
        xacts
        (fakePaginator Nothing
          (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionsGet))
      , _accountTransactionByIdGet = \transactionId ->
          getTransactionDetailForAccountTransaction accountId transactionId >>= \xacts -> bankStandardResponse
            xacts
            ((links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionByIdGet) transactionId)
      , _accountDirectDebitsGet    = getDirectDebitsForAccount accountId >>= \dds -> bankPaginatedResponse
        dds
        (fakePaginator Nothing
          (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountDirectDebitsGet))
      }
    }
