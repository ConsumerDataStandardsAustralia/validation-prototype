{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.Banking.AccountsTest where

import Data.Functor.Identity (Identity)
import Test.Tasty            (TestTree)
import Test.Tasty.HUnit      (testCase, (@?=))
import Waargonaut.Decode     (Decoder)
import Waargonaut.Encode     (Encoder)
import Waargonaut.Generic    (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip       (roundTripTest)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Tag

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
  "AccountTransactionResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountTransactionsResponse.golden.json"

test_roundTripAccountTransactionDetailResponse :: TestTree
test_roundTripAccountTransactionDetailResponse = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity AccountTransactionDetailResponse))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity AccountTransactionDetailResponse))))
  "AccountTransactionDetailResponseRoundtrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/accountTransactionDetailResponse.golden.json"
