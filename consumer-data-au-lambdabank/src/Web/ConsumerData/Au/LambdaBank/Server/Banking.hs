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

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.Server.Internal
    (LambdaBankM, bankPaginatedResponse, bankStandardResponse)

bankingServer :: ToServant BankingApi (AsServerT LambdaBankM)
bankingServer = genericServerT BankingApi
    { _bankingAccounts = genericServerT AccountsApi
      { _accountsGet = \pMay -> bankPaginatedResponse
        testAccounts
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsGet))
      , _accountsBalancesGet = \pMay -> bankPaginatedResponse
        testBalances
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
      , _accountsBalancesPost = bankStandardResponse
        testBalances
        (links^.bankingLinks.bankingAccountsLinks.accountsBalancesPost)
      , _accountsTransactionsGet = \pMay -> bankPaginatedResponse
        testAccountsTransactions
        (fakePaginator pMay (const $ (links^.bankingLinks.bankingAccountsLinks.accountsTransactionsGet) pMay))
      , _accountsTransactionsPost = bankPaginatedResponse
        testAccountsTransactions
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsTransactionsPost))
      , _accountsDirectDebitsGet = \pMay -> bankPaginatedResponse
        testDirectDebitAuthorisations
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsGet))
      , _accountsDirectDebitsPost = bankPaginatedResponse
        testDirectDebitAuthorisations
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsPost))
      , _accountsById = \accountId -> genericServerT AccountApi
        { _accountGet                = bankStandardResponse
          testAccountDetail
          (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountGet)
        , _accountTransactionsGet    = bankPaginatedResponse
          testAccountTransactions
          (fakePaginator Nothing
           (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionsGet))
        , _accountTransactionByIdGet = \transactionId -> bankStandardResponse
          testAccountTransactionDetail
          ((links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionByIdGet) transactionId)
        , _accountDirectDebitsGet    = bankPaginatedResponse
          testDirectDebitAuthorisations
          (fakePaginator Nothing
           (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountDirectDebitsGet))
        }
      }
    , _bankingPayees = genericServerT PayeesApi
      { _payeesGet = bankPaginatedResponse
        testPayees
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingPayeesLinks.payeesGet))
      , _payeesByIdGet = \_payeeId -> bankStandardResponse
        testPayeeDetail
        (links^.bankingLinks.bankingPayeesLinks.payeesByIdGet $ _payeeId)
      }
    , _bankingProducts = genericServerT ProductsApi
      { _productsGet = bankPaginatedResponse
        [testProduct]
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingProductsLinks.productsGet))
      , _productsByIdGet = \_productId -> bankStandardResponse
        testProductDetail
        (links^.bankingLinks.bankingProductsLinks.productsByIdGet $ _productId)
      }
    }
