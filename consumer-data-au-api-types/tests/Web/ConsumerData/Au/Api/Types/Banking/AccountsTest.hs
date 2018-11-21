{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.Banking.AccountsTest where

import Control.Lens

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity  (Identity)
import Test.Tasty             (TestTree)
import Test.Tasty.HUnit       (testCase, (@?=))
import Text.URI.QQ            (uri)
import Waargonaut.Decode      (Decoder)
import Waargonaut.Encode      (Encoder)
import Waargonaut.Generic       (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip          (roundTripTest)

import Web.ConsumerData.Au.Api.Client
import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Tag
import Web.ConsumerData.Au.Api.Types.FakeServer

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

test_roundTripAccounts :: TestTree
test_roundTripAccounts = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountsGetResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountsGetResponse))))
  "AccountsGetRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountsGet.golden.json"

test_roundTripAccountTransactionsResponse :: TestTree
test_roundTripAccountTransactionsResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountTransactionsResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountTransactionsResponse))))
  "AccountTransactionResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountTransactionsResponse.golden.json"

test_roundTripAccountTransactionDetailResponse :: TestTree
test_roundTripAccountTransactionDetailResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountTransactionDetailResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountTransactionDetailResponse))))
  "AccountTransactionDetailResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountTransactionDetailResponse.golden.json"
