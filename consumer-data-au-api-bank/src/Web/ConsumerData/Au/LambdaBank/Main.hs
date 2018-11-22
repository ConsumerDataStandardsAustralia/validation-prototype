{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.LambdaBank.Main where

import Web.ConsumerData.Au.Api.Types

{--
Notes: this is just moving the FakeServer from the types tests across for now.
We will split it apart and make it into a real server with config and a database
and not all in one file. Don't be too upset
that it looks heinous for now!
--}

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Control.Concurrent       (forkIO, killThread)
import Control.Exception        (bracket, throwIO)
import Country.Identifier       (australia)
import Data.Profunctor          (lmap)
import Data.List.NonEmpty       (NonEmpty((:|)))
import Data.Maybe               (fromMaybe)
import Data.Time (fromGregorian, UTCTime(..))
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant.API.Generic      (ToServant)
import Servant.Links            (Link)
import Servant.Server           (serve)
import Servant.Server.Generic   (AsServer, genericServer)
import Text.URI                 (Authority (..))
import Text.URI.QQ              (host, scheme)

import Web.ConsumerData.Au.LambdaBank.FakeData

server :: LinkQualifier -> ToServant Api AsServer
server lq = genericServer Api
  { _common = genericServer CommonApi
    { _customer = genericServer CustomerApi
      { _customerBriefGet = pure $ mkStandardResponse
        --(CustomerPerson testPerson)
        (CustomerOrganisation testOrganisation)
        lq
        (links ^.commonLinks.customerLinks.customerBriefGet)
      , _customerDetailsGet = pure $ mkStandardResponse
        (CustomerDetailPerson testPersonDetail)
        --(CustomerDetailOrganisation testOrganisationDetail)
        lq
        (links ^.commonLinks.customerLinks.customerDetailsGet)
      }
    }
  , _banking = genericServer BankingApi
    { _bankingAccounts = genericServer AccountsApi
      { _accountsGet = \pMay -> pure $ mkPaginatedResponse
        testAccounts
        lq
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsGet))
      , _accountsBalancesGet = \pMay -> pure $ mkPaginatedResponse
        testBalances
        lq
        (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
      , _accountsBalancesPost = pure $ mkPaginatedResponse
        testBalances
        lq
        (fakePaginator Nothing (links^.bankingLinks.bankingAccountsLinks.accountsBalancesGet))
      , _accountsTransactionsGet = \pMay -> pure $ mkPaginatedResponse testAccountsTransactions lq (fakePaginator pMay (const $ (links^.bankingLinks.bankingAccountsLinks.accountsTransactionsGet) pMay))
      , _accountsTransactionsPost = pure $ mkPaginatedResponse testAccountsTransactions lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsTransactionsPost))
      , _accountsDirectDebitsGet = \pMay -> pure $ mkPaginatedResponse testDirectDebitAuthorisations lq (fakePaginator pMay (links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsGet))
      , _accountsDirectDebitsPost = pure $ mkPaginatedResponse testDirectDebitAuthorisations lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsDirectDebitsPost))
      , _accountsById = \accountId -> genericServer AccountApi
        { _accountGet                = pure $ mkStandardResponse testAccountDetail lq (links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountGet)
        , _accountTransactionsGet    = pure $ mkPaginatedResponse testAccountTransactions lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionsGet))
        , _accountTransactionByIdGet = \transactionId -> pure $ mkStandardResponse testAccountTransactionDetail lq ((links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountTransactionByIdGet) transactionId)
        , _accountDirectDebitsGet    = pure $ mkPaginatedResponse
        testDirectDebitAuthorisations
        lq
        (fakePaginator Nothing (const $ links^.bankingLinks.bankingAccountsLinks.accountsByIdLinks.to ($accountId).accountDirectDebitsGet))
        }
      }
    , _bankingPayees = genericServer PayeesApi
      { _payeesGet = pure $ mkPaginatedResponse testPayees lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingPayeesLinks.payeesGet))
      , _payeesByIdGet = \_payeeId -> pure $ mkStandardResponse testPayeeDetail lq (links^.bankingLinks.bankingPayeesLinks.payeesByIdGet $ _payeeId)
      }
    , _bankingProducts = genericServer ProductsApi
      { _productsGet = pure $ mkPaginatedResponse [testProduct] lq (fakePaginator Nothing (const $ links^.bankingLinks.bankingProductsLinks.productsGet))
      , _productsByIdGet = \_productId -> pure $ mkStandardResponse testProductDetail lq (links^.bankingLinks.bankingProductsLinks.productsByIdGet $ _productId)
      }
    }
  }

app :: LinkQualifier -> Application
app = serve api . server

runServer :: Int -> IO ()
runServer port = run port (app $ fakeQualifier)
  where
    fakeQualifier = LinkQualifier
      [scheme|http|]
      (Authority
       { authUserInfo = Nothing
       , authHost     = [host|localhost|]
       , authPort     = Just $ fromIntegral port
       })
      []

main :: IO ()
main = runServer 8000
