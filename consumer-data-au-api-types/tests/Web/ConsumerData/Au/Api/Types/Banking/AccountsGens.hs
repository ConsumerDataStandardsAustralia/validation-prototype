module Web.ConsumerData.Au.Api.Types.Banking.AccountsGens where

import           Data.Text.Gens (textGen)
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens
    (amountStringGen, asciiStringGen, currencyAmountGen, currencyStringGen,
    dateTimeStringGen)


accountIdsGen :: Gen AccountIds
accountIdsGen = AccountIds
  <$> Gen.list (Range.linear 0 3) accountIdGen

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
  [ BalanceDeposit <$>
      ( DepositBalanceType
          <$> currencyAmountGen
          <*> currencyAmountGen )
  , BalanceLending <$>
      ( LendingBalanceType
          <$> currencyAmountGen
          <*> currencyAmountGen
          <*> currencyAmountGen
          <*> Gen.maybe currencyAmountGen )
  , BalancePurses <$>
      ( MultiCurrencyPursesType
          <$> Gen.list (Range.linear 0 3) currencyAmountGen )
  ]

accountBalanceGen :: Gen AccountBalance
accountBalanceGen = AccountBalance
  <$> accountIdGen
  <*> balanceGen

-- -----

transactionGen :: Gen Transaction
transactionGen = Transaction
  <$> Gen.maybe transactionIdGen
  <*> Gen.bool
  <*> transactionStatusGen
  <*> textGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> textGen

transactionIdGen :: Gen TransactionId
transactionIdGen = TransactionId <$> asciiStringGen

transactionStatusGen :: Gen TransactionStatus
transactionStatusGen = Gen.enumBounded


transactionDetail :: Gen TransactionDetail
transactionDetail = TransactionDetail
  <$> Gen.maybe transactionIdGen
  <*> transactionStatusGen
  <*> textGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> textGen
  <*> Gen.maybe transactionExtendedDataGen

transactionExtendedDataGen :: Gen TransactionExtendedData
transactionExtendedDataGen = TransactionExtendedData
  <$> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe transactionExtendedDataExtensionTypeGen
  <*> transactionExtendedDataServiceGen

transactionExtendedDataExtensionTypeGen :: Gen TransactionExtendedDataExtensionType
transactionExtendedDataExtensionTypeGen = Gen.choice
  [ TEDExtendedDescription <$> textGen
  ]

transactionExtendedDataServiceGen :: Gen TransactionExtendedDataService
transactionExtendedDataServiceGen = Gen.enumBounded


bulkTransactionGen :: Gen BulkTransaction
bulkTransactionGen = BulkTransaction
  <$> accountIdGen
  <*> Gen.maybe transactionIdGen
  <*> Gen.bool
  <*> bulkTransactionStatusGen
  <*> textGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> textGen

bulkTransactionStatusGen :: Gen BulkTransactionStatus
bulkTransactionStatusGen = Gen.enumBounded
