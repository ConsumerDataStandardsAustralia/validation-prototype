{-# LANGUAGE QuasiQuotes #-}
module Web.ConsumerData.Au.LambdaBank.Banking.AccountsTest where

import Control.Lens
import Web.ConsumerData.Au.Api.Client
import Web.ConsumerData.Au.Api.Types

import Control.Monad.IO.Class (liftIO)
import Data.Text              (pack)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       (testCase, (@?=))
import Text.URI.QQ            (uri)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.Model
    (filterBalancesByAccountIds, filterDirectDebitsByAccountIds,
    filterTransactionsByAccountIds)
import Web.ConsumerData.Au.LambdaBank.WithServer

test_accounts :: [TestTree]
test_accounts =
  [ testCase "/banking/accounts test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsGet . to (\f -> f Nothing Nothing Nothing Nothing Nothing)
      liftIO $ res @?= Response testAccounts
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts?page=1|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "/banking/accounts test with page 2" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsGet . to (\f -> f Nothing Nothing Nothing (Just (PageNumber 2)) Nothing)
      liftIO $ res @?= Response testAccounts
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts?page=2|]
         (Just [uri|http://localhost:1337/banking/accounts?page=1|])
         (Just [uri|http://localhost:1337/banking/accounts?page=1|])
         Nothing
         Nothing
        )
        (MetaPaginated 0 2)
  , testCase "/banking/accounts/{accountId} test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsByIdClient . to ($ AccountId (AsciiString (pack "12345"))) . accountGet
      liftIO $ res @?= Response testAccountDetail
        (LinksStandard [uri|http://localhost:1337/banking/accounts/12345|])
        MetaStandard
  , testCase "/banking/accounts/balances test with page" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsBalancesGet . to (\f -> f Nothing Nothing Nothing (Just (PageNumber 2)) Nothing)
      liftIO $ res @?= Response testBalances
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/balances?page=2|]
         (Just [uri|http://localhost:1337/banking/accounts/balances?page=1|])
         (Just [uri|http://localhost:1337/banking/accounts/balances?page=1|])
         Nothing
         Nothing
        )
        (MetaPaginated 0 2)
  , testCase "/banking/accounts/balances test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsBalancesGet . to (\f -> f Nothing Nothing Nothing Nothing Nothing)
      liftIO $ res @?= Response testBalances
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/balances?page=1|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "/banking/accounts/transactions test with page 2" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsTransactionsGet . to (\f -> f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just (PageNumber 2)) Nothing)
      liftIO $ res @?= Response testAccountsTransactions
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/transactions?page=2|]
         (Just [uri|http://localhost:1337/banking/accounts/transactions?page=1|])
         (Just [uri|http://localhost:1337/banking/accounts/transactions?page=1|])
         Nothing
         Nothing
        )
        (MetaPaginated 0 2)
  , testCase "/banking/accounts/{accountId}/transactions test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsByIdClient . to ($ AccountId (AsciiString (pack "12345"))) . accountTransactionsGet . to (\f -> f Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
      liftIO $ res @?= Response testAccountTransactions
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/12345/transactions?page=1|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "/banking/accounts/{accountId}/transactions test with page 2" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsByIdClient . to ($ AccountId (AsciiString (pack "12345"))) . accountTransactionsGet . to (\f -> f Nothing Nothing Nothing Nothing Nothing (Just (PageNumber 2)) Nothing)
      liftIO $ res @?= Response testAccountTransactions
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/12345/transactions?page=2|]
         (Just [uri|http://localhost:1337/banking/accounts/12345/transactions?page=1|])
         (Just [uri|http://localhost:1337/banking/accounts/12345/transactions?page=1|])
         Nothing
         Nothing
        )
        (MetaPaginated 0 2)
  , testCase "/banking/accounts/{accountId}/transactions/{transactionId} test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsByIdClient . to ($ AccountId (AsciiString (pack "12345"))) . accountTransactionByIdGet . to ($ TransactionId (AsciiString (pack "6789")))
      liftIO $ res @?= Response testAccountTransactionsDetail
        (LinksStandard [uri|http://localhost:1337/banking/accounts/12345/transactions/6789|])
        MetaStandard
  ]

test_accountsPost :: [TestTree]
test_accountsPost =
  [ testCase "POST /banking/accounts/balances test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsBalancesPost . to ($ requestAccountIds)
      liftIO $ res @?= Response (filterBalancesByAccountIds testAccountIds testBalances)
        (LinksStandard [uri|http://localhost:1337/banking/accounts/balances|])
        MetaStandard
  , testCase "POST /banking/accounts/transactions test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsTransactionsPost . to (\f -> f Nothing Nothing Nothing Nothing Nothing requestAccountIds Nothing Nothing)
      liftIO $ res @?= Response (filterTransactionsByAccountIds testAccountIds testAccountsTransactions)
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/transactions?page=1|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "POST /banking/accounts/transactions test with page 2" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsTransactionsPost . to (\f -> f Nothing Nothing Nothing Nothing Nothing requestAccountIds (Just (PageNumber 2)) Nothing)
      liftIO $ res @?= Response (filterTransactionsByAccountIds testAccountIds testAccountsTransactions)
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/transactions?page=2|]
        (Just [uri|http://localhost:1337/banking/accounts/transactions?page=1|])
        (Just [uri|http://localhost:1337/banking/accounts/transactions?page=1|])
         Nothing
         Nothing
        )
        (MetaPaginated 0 2)
  , testCase "POST /banking/accounts/direct-debits test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsDirectDebitsPost . to (\f -> f requestAccountIds Nothing Nothing)
      liftIO $ res @?= Response (filterDirectDebitsByAccountIds testAccountIds testDirectDebitAuthorisations)
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/direct-debits?page=1|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  ]
  where
    requestAccountIds :: RequestAccountIds
    requestAccountIds = mkStandardRequest (AccountIds [AccountId (AsciiString (pack "12345")), AccountId (AsciiString (pack "12347"))])
