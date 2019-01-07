{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.Banking.AccountsTest where

import Control.Lens

import Data.Functor.Identity (Identity)
import Data.Time             (UTCTime(UTCTime), fromGregorian)
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
  [ paginatedLinkTest "Get Accounts no Params"
    ((alinks^.accountsGet) Nothing Nothing Nothing) [uri|http://localhost/banking/accounts|]
  , paginatedLinkTest "Get Accounts all params"
    ((alinks^.accountsGet) (Just AccountOpen) (Just AccountOwned) (Just PCTravelCard))
    [uri|http://localhost/banking/accounts?open-status=OPEN&is-owned=OWNED&product-category=TRAVEL_CARD|]
  , paginatedLinkTest "Get Bulk Balances no params"
    ((alinks^.accountsBalancesGet) Nothing Nothing Nothing) [uri|http://localhost/banking/accounts/balances|]
  , paginatedLinkTest "Get Bulk Balances all params"
    ((alinks^.accountsBalancesGet) (Just AccountOpen) (Just AccountOwned) (Just PCTravelCard))
    [uri|http://localhost/banking/accounts/balances?open-status=OPEN&is-owned=OWNED&product-category=TRAVEL_CARD|]
  , linkTest "Get Balances For Specific Accounts"
    (alinks^.accountsBalancesPost) [uri|http://localhost/banking/accounts/balances|]
  , paginatedLinkTest "Get Bulk Transactions no Params"
    ((alinks^.accountsTransactionsGet) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) [uri|http://localhost/banking/accounts/transactions|]
  , paginatedLinkTest "Get Bulk Transactions all Params"
    ((alinks^.accountsTransactionsGet)
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 3) 1337))
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 4) 23828))
     (Just (AmountString "100.00"))
     (Just (AmountString "500.00"))
     (Just "Lucky Egg")
     (Just AccountOpen)
     (Just AccountOwned)
     (Just PCTravelCard)
    ) [uri|http://localhost/banking/accounts/transactions?start-time=2019-01-03T00:22:17Z&end-time=2019-01-04T06:37:08Z&min-amount=100.00&max-amount=500.00&text=Lucky+Egg&open-status=OPEN&is-owned=OWNED&product-category=TRAVEL_CARD|]
  , paginatedLinkTest "Get Transactions for Specific Accounts no params"
    ((alinks^.accountsTransactionsPost) Nothing Nothing Nothing Nothing Nothing) [uri|http://localhost/banking/accounts/transactions|]
  , paginatedLinkTest "Get Transactions for Specific Accounts all params"
    ((alinks^.accountsTransactionsPost)
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 3) 1337))
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 4) 23828))
     (Just (AmountString "100.00"))
     (Just (AmountString "500.00"))
     (Just "Lucky Egg")
    )[uri|http://localhost/banking/accounts/transactions?start-time=2019-01-03T00:22:17Z&end-time=2019-01-04T06:37:08Z&min-amount=100.00&max-amount=500.00&text=Lucky+Egg|]
  , paginatedLinkTest "Get Bulk Direct Debits no params"
    ((alinks^.accountsDirectDebitsGet) Nothing Nothing) [uri|http://localhost/banking/accounts/direct-debits|]
  , paginatedLinkTest "Get Bulk Direct Debits all params"
    ((alinks^.accountsDirectDebitsGet) (Just $ AccountOwned) (Just $ PCTravelCard)) [uri|http://localhost/banking/accounts/direct-debits?is-owned=OWNED&product-category=TRAVEL_CARD|]
  , linkTest "Get Direct Debits for Specific Accounts"
    (alinks^.accountsDirectDebitsPost) [uri|http://localhost/banking/accounts/direct-debits|]
  , linkTest "Get Account Detail"
    (accLinks^.accountGet) [uri|http://localhost/banking/accounts/123|]
  , paginatedLinkTest "Get Transactions For Account no params"
    ((accLinks^.accountTransactionsGet) Nothing Nothing Nothing Nothing Nothing)[uri|http://localhost/banking/accounts/123/transactions|]
  , paginatedLinkTest "Get Transactions For Account all params"
    ((accLinks^.accountTransactionsGet)
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 3) 1337))
     (Just (DateTimeString $ UTCTime (fromGregorian 2019 1 4) 23828))
     (Just (AmountString "100.00"))
     (Just (AmountString "500.00"))
     (Just "Lucky Egg")
    )[uri|http://localhost/banking/accounts/123/transactions?start-time=2019-01-03T00:22:17Z&end-time=2019-01-04T06:37:08Z&min-amount=100.00&max-amount=500.00&text=Lucky+Egg|]
  , linkTest "Get Transaction Detail"
    (accLinks^.accountTransactionByIdGet.to ($ TransactionId (AsciiString "456"))) [uri|http://localhost/banking/accounts/123/transactions/456|]
  , paginatedLinkTest "Get Direct Debits for Account"
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
