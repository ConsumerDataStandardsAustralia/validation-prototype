{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.Server.Banking where

{--
Notes: this is just moving the FakeServer from the types tests across for now.
We will split it apart and make it into a real server with config and a database
and not all in one file. Don't be too upset
that it looks heinous for now!
--}

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Servant.API.Generic      (ToServant)
import Servant.Server.Generic   (AsServerT, genericServerT)

import Web.ConsumerData.Au.LambdaBank.FakeData (fakePaginator)
import Web.ConsumerData.Au.LambdaBank.Model
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankPaginatedResponse, bankStandardResponse)

bankingServer :: ToServant BankingApi (AsServerT LambdaBankM)
bankingServer = genericServerT BankingApi
    { _bankingAccounts = genericServerT AccountsApi
      { _accountsGet = \pMay -> getAccounts >>= \as -> bankPaginatedResponse
        as
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsGet))
      , _accountsBalancesGet = \pMay -> getBalancesAll >>= \bs -> bankPaginatedResponse
        bs
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
      , _accountsBalancesPost = getBalancesForAccounts (error "TODO") >>= \bs -> bankPaginatedResponse
        bs
        (fakePaginator Nothing (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
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
    , _bankingPayees = genericServerT PayeesApi
      { _payeesGet = getPayeesAll >>= \ps -> bankPaginatedResponse
        ps
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingPayeesLinks.payeesGet))
      , _payeesByIdGet = \payeeId -> getPayeeDetail payeeId >>= \pd -> bankStandardResponse
        pd
        (links^.bankingLinks.bankingPayeesLinks.payeesByIdGet $ payeeId)
      }
    , _bankingProducts = genericServerT ProductsApi
      { _productsGet = getProductsAll >>= \ps -> bankPaginatedResponse
        ps
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingProductsLinks.productsGet))
      , _productsByIdGet = \productId -> getProductDetail productId >>= \pd -> bankStandardResponse
        pd
        (links^.bankingLinks.bankingProductsLinks.productsByIdGet $ productId)
      }
    }
