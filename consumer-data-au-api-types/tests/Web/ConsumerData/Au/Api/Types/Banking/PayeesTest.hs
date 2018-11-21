{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE QuasiQuotes       #-}

module Web.ConsumerData.Au.Api.Types.Banking.PayeesTest where

import Control.Lens
import Control.Monad.IO.Class   (liftIO)
import Data.Functor.Identity    (Identity)
import Data.Tagged              (Tagged)
import Test.Tasty               (TestTree)
import Test.Tasty.HUnit         (testCase, (@?=))
import Text.URI.QQ              (uri)
import Waargonaut.Decode        (Decoder)
import Waargonaut.Encode        (Encoder)
import Waargonaut.Generic       (mkDecoder, mkEncoder, untag)
import WaargoRoundTrip          (roundTripTest)

import Web.ConsumerData.Au.Api.Client (apiClient, bankingClient, payeesClient)
import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Tag
import Web.ConsumerData.Au.Api.Types.FakeServer


test_payees :: [TestTree]
test_payees =
  [ testCase "payees test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . payeesClient . payeesGet
      liftIO $ res @?= Response testPayees
        (LinksPaginated
         [uri|http://localhost:1337/banking/payees|]
         Nothing
         Nothing
         Nothing
         Nothing
        )
        (MetaPaginated 0 1)
  , testCase "payeesById test" . withServer 1337 $ do
      res <- apiClient ^. bankingClient . payeesClient . payeesByIdGet . to ($ PayeeId "5")
      liftIO $ res @?= Response testPayeeDetail
        (LinksStandard [uri|http://localhost:1337/banking/payees/5|])
        MetaStandard
  ]

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
