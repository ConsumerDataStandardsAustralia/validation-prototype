{-# LANGUAGE QuasiQuotes #-}
module Web.ConsumerData.Au.LambdaBank.Banking.AccountsTest where

import Control.Lens
import Web.ConsumerData.Au.Api.Client
import Web.ConsumerData.Au.Api.Types

import Control.Monad.IO.Class (liftIO)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       (testCase, (@?=))
import Text.URI.QQ            (uri)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.WithServer

test_accounts :: [TestTree]
test_accounts =
  [ testCase "balances test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsBalancesGet . to ($ Just (PageNumber 2))
      liftIO $ res @?= Response testBalances
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/balances?page=2|]
         (Just [uri|http://localhost:1337/banking/accounts/balances?page=1|])
         (Just [uri|http://localhost:1337/banking/accounts/balances?page=1|])
         Nothing
         Nothing
        )
        (MetaPaginated 0 2)
  , testCase "balances test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . bankingAccountsClient . accountsBalancesGet . to ($ Nothing)
      liftIO $ res @?= Response testBalances
        (LinksPaginated
         [uri|http://localhost:1337/banking/accounts/balances?page=1|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  ]
