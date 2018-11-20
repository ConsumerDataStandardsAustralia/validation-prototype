{-# LANGUAGE TypeApplications #-}

module Web.ConsumerData.Au.Api.Types.Banking.PayeesTest where

import Data.Functor.Identity (Identity)
import Data.Tagged (Tagged)
import Test.Tasty               (TestTree)
import Waargonaut.Decode (Decoder)
import Waargonaut.Encode (Encoder)
import Waargonaut.Generic       (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip          (roundTripTest)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Tag

test_roundTripPayees :: TestTree
test_roundTripPayees = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity (PaginatedResponse Payees)))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity (PaginatedResponse Payees)))))
  "PayeesRoundTrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/PayeesTest/payees.golden.json"

test_roundTripPayeeDetail :: TestTree
test_roundTripPayeeDetail = roundTripTest
  (untag (mkDecoder :: (Tagged OB (Decoder Identity (StandardResponse PayeeDetail)))))
  (untag (mkEncoder :: (Tagged OB (Encoder Identity (StandardResponse PayeeDetail)))))
  "PayeeDetailRoundTrip"
  "tests/Web/ConsumerData/Au/Api/Types/Banking/PayeesTest/payeedetail.golden.json"
