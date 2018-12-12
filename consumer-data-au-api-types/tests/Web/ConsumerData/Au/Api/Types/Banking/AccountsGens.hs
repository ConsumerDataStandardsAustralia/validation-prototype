module Web.ConsumerData.Au.Api.Types.Banking.AccountsGens where

import           Data.Text.Gens (textGen)
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens
    (asciiStringGen, currencyAmountGen)


accountIdGen :: Gen AccountId
accountIdGen = AccountId <$> asciiStringGen

accountGen :: Gen Account
accountGen = Account
  <$> accountIdGen
  <*> textGen
  <*> Gen.maybe textGen
  <*> maskedAccountNumberGen
  <*> Gen.maybe productCategoryGen
  <*> textGen
  <*> balanceGen

maskedAccountNumberGen :: Gen MaskedAccountNumber
maskedAccountNumberGen = MaskedAccountNumber <$> textGen

productCategoryGen :: Gen ProductCategory
productCategoryGen = Gen.enumBounded

balanceGen :: Gen Balance
balanceGen = Gen.choice
  [ Deposits
      <$> currencyAmountGen
      <*> currencyAmountGen
  , Lending
      <$> currencyAmountGen
      <*> currencyAmountGen
      <*> currencyAmountGen
      <*> Gen.maybe currencyAmountGen
  , Purses
      <$> Gen.list (Range.linear 0 3) currencyAmountGen
  ]
