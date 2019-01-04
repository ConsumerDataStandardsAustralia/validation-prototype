{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.RequestTest where

-- import Control.Lens
import Data.Functor.Identity (Identity)
import Test.Tasty            (TestTree)
import Test.Tasty.HUnit      (testCase, (@?=))
-- import Text.URI.QQ           (uri)
import Waargonaut.Decode     (Decoder)
import Waargonaut.Encode     (Encoder)
import Waargonaut.Generic    (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip       (roundTripTest)

import Web.ConsumerData.Au.Api.Types
-- import Web.ConsumerData.Au.Api.Types.LinkTestHelpers (linkTest)
import Web.ConsumerData.Au.Api.Types.Tag


test_mkAccountIdsRequest :: [TestTree]
test_mkAccountIdsRequest =
  [ testCase "with AccountIds" $
    (mkStandardRequest
     True
    )
    @?=
    (Request
     True
     Meta
    )
  , testCase "with AccountIds" $
    (mkStandardRequest
     (AccountIds (
      [ (AccountId (AsciiString "12345"))
      , (AccountId (AsciiString "12346"))
      ]))
    )
    @?=
    (Request
     (AccountIds (
      [ (AccountId (AsciiString "12345"))
      , (AccountId (AsciiString "12346"))
      ]))
     Meta
    )
  ]

test_roundTripPostRoutesWithRequestAccountIds :: [TestTree]
test_roundTripPostRoutesWithRequestAccountIds =
  [ roundTripTest
    (untag (mkDecoder :: (Tagged OB (Decoder Identity (StandardRequest AccountIds)))))
    (untag (mkEncoder :: (Tagged OB (Encoder Identity (StandardRequest AccountIds)))))
    " RequestAccountIds RoundTrip"
    "tests/Web/ConsumerData/Au/Api/Types/Banking/AccountsTest/requestAccountIds.golden.json"
  ]


-- test_accountPOSTLinks :: [TestTree]
-- test_accountLinks =
--   [ linkTest "Get Balances For Specific Accounts - POST"
--     (alinks^.accountsBalancesPost) [uri|http://localhost/banking/accounts/balances|]
--   , linkTest "Get Transactions for Specific Accounts - POST"
--     (alinks^.accountsTransactionsPost) [uri|http://localhost/banking/accounts/transactions|]
--   , linkTest "Get Direct Debits for Specific Accounts - POST"
--     (alinks^.accountsDirectDebitsPost) [uri|http://localhost/banking/accounts/direct-debits|]
--   ]
--   where
--     alinks   = links^.bankingLinks.bankingAccountsLinks
