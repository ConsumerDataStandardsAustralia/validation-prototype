module Web.ConsumerData.Au.Api.Types.Banking.DirectDebitsGens where

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import Data.Text.Gens                                          (textGen)
import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Banking.AccountsGens      (accountIdGen)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens
    (amountStringGen, dateTimeStringGen)


accountDirectDebitGen :: Gen AccountDirectDebit
accountDirectDebitGen = AccountDirectDebit
  <$> accountIdGen
  <*> Gen.maybe authorisedEntityGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe amountStringGen

authorisedEntityGen :: Gen AuthorisedEntity
authorisedEntityGen = AuthorisedEntity
  <$> textGen
  <*> textGen
  <*> Gen.maybe abnGen
  <*> Gen.maybe acnGen

abnGen :: Gen Abn
abnGen = Abn <$> textGen

acnGen :: Gen Acn
acnGen = Acn <$> textGen
