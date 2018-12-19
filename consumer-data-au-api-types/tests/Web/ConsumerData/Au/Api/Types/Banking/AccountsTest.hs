{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.Banking.AccountsTest where

import Control.Lens

import Data.Functor.Identity (Identity)
import Test.Tasty            (TestTree)
import Test.Tasty.HUnit      (testCase, (@?=))
import Text.URI.QQ           (uri)
import Waargonaut.Decode     (Decoder)
import Waargonaut.Encode     (Encoder)
import Waargonaut.Generic    (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip       (roundTripTest)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.LinkTestHelpers
    (linkTest, paginatedLinkTest)
import Web.ConsumerData.Au.Api.Types.Tag


-- TODO: Our routes don't take any query params or req bodies for the post.
-- More to test here later once it is there.
test_accountLinks :: [TestTree]
test_accountLinks =
  [ paginatedLinkTest "Get Accounts"
    (alinks^.accountsGet) [uri|http://localhost/banking/accounts|]
  , paginatedLinkTest "Get Bulk Balances"
    (alinks^.accountsBalancesGet) [uri|http://localhost/banking/accounts/balances|]
  , linkTest "Get Balances For Specific Accounts"
    (alinks^.accountsBalancesPost) [uri|http://localhost/banking/accounts/balances|]
  , paginatedLinkTest "Get Bulk Transactions"
    (alinks^.accountsTransactionsGet) [uri|http://localhost/banking/accounts/transactions|]
  , linkTest "Get Transactions for Specific Accounts"
    (alinks^.accountsTransactionsPost) [uri|http://localhost/banking/accounts/transactions|]
  , paginatedLinkTest "Get Bulk Direct Debits"
    (alinks^.accountsDirectDebitsGet) [uri|http://localhost/banking/accounts/direct-debits|]
  , linkTest "Get Direct Debits for Specific Accounts"
    (alinks^.accountsDirectDebitsPost) [uri|http://localhost/banking/accounts/direct-debits|]
  , linkTest "Get Account Detail"
    (accLinks^.accountGet) [uri|http://localhost/banking/accounts/123|]
  , linkTest "Get Transactions For Account"
    (accLinks^.accountTransactionsGet) [uri|http://localhost/banking/accounts/123/transactions|]
  , linkTest "Get Transaction Detail"
    (accLinks^.accountTransactionByIdGet.to ($ TransactionId (AsciiString "456"))) [uri|http://localhost/banking/accounts/123/transactions/456|]
  , linkTest "Get Direct Debits for Account"
    (accLinks^.accountDirectDebitsGet) [uri|http://localhost/banking/accounts/123/direct-debits|]
  ]
  where
    alinks   = links^.bankingLinks.bankingAccountsLinks
    accLinks = alinks^.accountsByIdLinks.to ($ AccountId (AsciiString "123"))

-- TODO This probably does what people expect, but it is poorly specified in the spec
test_maskAccountId :: [TestTree]
test_maskAccountId =
  [ testCase "too short" $ do
    maskAccountId (AccountId (AsciiString "")) @?= (MaskedAccountNumber "")
    maskAccountId (AccountId (AsciiString "123")) @?= (MaskedAccountNumber "123")
    maskAccountId (AccountId (AsciiString "1234")) @?= (MaskedAccountNumber "1234")
  , testCase "masks numbers" $ do
    maskAccountId (AccountId (AsciiString "123456789")) @?= (MaskedAccountNumber "XXXXX6789")
  , testCase "doesn't mask non numbers" $ do
    maskAccountId (AccountId (AsciiString "12 345-6789")) @?= (MaskedAccountNumber "XX XXX-6789")
  ]

test_accounts :: [TestTree]
test_accounts =
  [ roundTripTest
    (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountsGetResponse))))
    (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountsGetResponse))))
    "AccountsGetRoundtrip"
    "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountsGet.golden.json"
--  , roundTripProp accountsGen accountsDecoder accountsEncoder "AccountsRoundTrip"
  ]

test_roundTripAccountTransactionsResponse :: TestTree
test_roundTripAccountTransactionsResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountTransactionsResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountTransactionsResponse))))
  "AccountTransactionsResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountTransactionsResponse.golden.json"

test_roundTripAccountTransactionDetailResponse :: TestTree
test_roundTripAccountTransactionDetailResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountTransactionDetailResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountTransactionDetailResponse))))
  "AccountTransactionDetailResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountTransactionDetailResponse.golden.json"

test_roundTripAccountsTransactionsResponse :: TestTree
test_roundTripAccountsTransactionsResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountsTransactionsResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountsTransactionsResponse))))
  "AccountsTransactionsResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountBulkTransactionsResponse.golden.json"



test_roundTripAccountDirectDebitsResponse :: TestTree
test_roundTripAccountDirectDebitsResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountDirectDebitsResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountDirectDebitsResponse))))
  "AccountDirectDebitsGetRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountDirectDebits.golden.json"

test_roundTripAccountBulkBalanceResponse :: TestTree
test_roundTripAccountBulkBalanceResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountBulkBalanceResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountBulkBalanceResponse))))
  "AccountBulkBalanceResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountBulkBalance.golden.json"

test_roundTripAccountBalanceByIdsResponse :: TestTree
test_roundTripAccountBalanceByIdsResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountBalanceByIdsResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountBalanceByIdsResponse))))
  "AccountBalanceByIdsResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountBalanceByIds.golden.json"

test_roundTripAccountsByIdRoute :: TestTree
test_roundTripAccountsByIdRoute = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountByIdResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountByIdResponse))))
  "AccountsByIdRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountById.golden.json"
