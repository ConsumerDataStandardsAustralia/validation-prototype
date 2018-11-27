{-# LANGUAGE QuasiQuotes #-}
module Web.ConsumerData.Au.Api.Types.Banking.AccountsTest where

import Data.Functor.Identity (Identity)
import Test.Tasty            (TestTree)
import Waargonaut.Decode     (Decoder)
import Waargonaut.Encode     (Encoder)
import Waargonaut.Generic    (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip       (roundTripTest)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Tag

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
