module Web.ConsumerData.Au.Api.Types.Banking.AccountsGens where

import           Control.Monad.Catch (MonadThrow)
import           Data.Text.Gens      (textGen)
import           Hedgehog
import           Hedgehog            (MonadGen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Text.URI.Gens       (genUri)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.DepositRate
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Discount
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Feature
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Fee
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.LendingRate
import Web.ConsumerData.Au.Api.Types.Banking.ProductsGens
    (enumProductCategoryGen)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens
import Web.ConsumerData.Au.Api.Types.Data.Gens
    (physicalAddressGen)

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
  <*> Gen.maybe accOpenStatusGen
  <*> Gen.maybe Gen.bool
  <*> enumProductCategoryGen
  <*> textGen

maskedAccountNumberGen :: Gen MaskedAccountNumber
maskedAccountNumberGen = MaskedAccountNumber <$> textGen

accOpenStatusGen :: Gen AccOpenStatus
accOpenStatusGen = Gen.enumBounded


accountDetailGen :: (MonadGen m, MonadThrow m) => m AccountDetail
accountDetailGen = AccountDetail
  <$> Gen.lift accountGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe specificAccountGen
  <*> Gen.maybe accountFeaturesGen
  <*> Gen.maybe accountFeesGen
  <*> Gen.maybe accountDepositRatesGen
  <*> Gen.maybe accountLendingRatesGen
  <*> Gen.maybe (Gen.lift physicalAddressGen)

specificAccountGen :: (MonadGen m) => m SpecificAccount
specificAccountGen = Gen.lift $ Gen.choice
  [ TermDeposit <$> termDepositAccountTypeGen
  , CreditCard <$> creditCardAccountTypeGen
  , Loan <$> loanAccountTypeGen
  ]

termDepositAccountTypeGen :: Gen TermDepositAccountType
termDepositAccountTypeGen = TermDepositAccountType
  <$> dateStringGen
  <*> dateStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> maturityInstructionsGen

maturityInstructionsGen :: Gen MaturityInstructions
maturityInstructionsGen = Gen.enumBounded

creditCardAccountTypeGen :: Gen CreditCardAccountType
creditCardAccountTypeGen = CreditCardAccountType
  <$> amountStringGen
  <*> amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> dateStringGen

loanAccountTypeGen :: Gen LoanAccountType
loanAccountTypeGen = LoanAccountType
  <$> Gen.maybe dateStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> Gen.maybe dateStringGen
  <*> Gen.maybe dateStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> Gen.maybe Gen.bool
  <*> Gen.maybe (Gen.list (Range.linear 0 3) accountIdGen)
  <*> Gen.maybe durationStringGen
  <*> Gen.maybe repaymentTypeGen

repaymentTypeGen :: Gen RepaymentType
repaymentTypeGen = Gen.enumBounded


accountFeaturesGen :: (MonadGen m) => m AccountFeatures
accountFeaturesGen = AccountFeatures
  <$> Gen.list (Range.linear 0 10) accountFeatureTypeGen

accountFeatureTypeGen :: (MonadGen m) => m AccountFeatureType
accountFeatureTypeGen = Gen.choice
  [ pure AFeatureCardAcess
  , AFeatureAdditionalCards <$> intGen
  , pure AFeatureUnlimitedTxns
  , AFeatureFreeTxns <$> intGen
  , AFeatureFreeTxnsAllowance <$> amountStringGen
  , AFeatureLoyaltyProgram <$> textGen
  , pure AFeatureOffset
  , pure AFeatureOverdraft
  , pure AFeatureRedraw
  , AFeatureInsurance <$> textGen
  , pure AFeatureBalanceTransfers
  , AFeatureInterestFree <$> durationStringGen
  , AFeatureInterestFreeTransfers <$> durationStringGen
  , AFeatureDigitalWallet <$> textGen
  , pure AFeatureDigitalBanking
  , pure AFeatureNppPayid
  , pure AFeatureNppEnabled
  , pure AFeatureDonateInterest
  , AFeatureBillPayment <$> textGen
  ]

accountFeesGen :: (MonadGen m, MonadThrow m) => m AccountFees
accountFeesGen = AccountFees
  <$> (Gen.list (Range.linear 0 10) accountFeeGen)

accountFeeGen :: (MonadGen m, MonadThrow m) => m AccountFee
accountFeeGen = AccountFee
  <$> textGen
  <*> accountFeeTypeGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe rateStringGen
  <*> Gen.maybe rateStringGen
  <*> Gen.maybe currencyStringGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genUri
  <*> Gen.maybe accountDiscountsGen

accountFeeTypeGen :: (MonadGen m) => m AccountFeeType
accountFeeTypeGen = Gen.choice
  [ AFeePeriodicPeriodic <$> durationStringGen
  , AFeePeriodicTransaction <$> textGen
  , pure AFeePeriodicExit
  , pure AFeePeriodicOverdraw
  , AFeePeriodicMinBalance <$> durationStringGen
  , pure AFeePeriodicRedraw
  , pure AFeePeriodicChequeCash
  , pure AFeePeriodicChequeStop
  , pure AFeePeriodicChequeBook
  , pure AFeePeriodicCardReplace
  , pure AFeePeriodicPaperStatement
  , AFeePeriodicOtherEvent <$> textGen
  ]

accountDepositRatesGen :: (MonadGen m, MonadThrow m) => m AccountDepositRates
accountDepositRatesGen = AccountDepositRates
  <$> Gen.list (Range.linear 0 10) accountDepositRateGen

accountDepositRateGen :: (MonadGen m, MonadThrow m) => m AccountDepositRate
accountDepositRateGen = AccountDepositRate
  <$> accountDepositRateTypeGen
  <*> rateStringGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genUri

accountDepositRateTypeGen :: (MonadGen m) => m AccountDepositRateType
accountDepositRateTypeGen =
  Gen.choice
  [ ADepositRateTypeFixed <$> dateTimeStringGen
  , ADepositRateTypeBonus <$> textGen
  , ADepositRateTypeBundleBonus <$> textGen
  , pure ADepositRateTypeVariable
  , ADepositRateTypeIntroductory <$> dateTimeStringGen
  ]

accountDiscountsGen :: (MonadGen m) => m AccountDiscounts
accountDiscountsGen = AccountDiscounts
  <$> Gen.list (Range.linear 0 10) accountDiscountGen

accountDiscountGen :: (MonadGen m) => m AccountDiscount
accountDiscountGen = AccountDiscount
  <$> textGen
  <*> accountDiscountTypeGen
  <*> amountStringGen

accountDiscountTypeGen :: (MonadGen m) => m AccountDiscountType
accountDiscountTypeGen =
  Gen.choice
  [ ADiscountBalance <$> amountStringGen
  , ADiscountDeposits <$> amountStringGen
  , ADiscountPayments <$> amountStringGen
  , ADiscountBundle <$> textGen
  ]

accountLendingRatesGen :: (MonadGen m, MonadThrow m) => m AccountLendingRates
accountLendingRatesGen = AccountLendingRates
  <$> Gen.list (Range.linear 0 10) accountLendingRateGen

accountLendingRateGen :: (MonadGen m, MonadThrow m) => m AccountLendingRate
accountLendingRateGen = AccountLendingRate
  <$> accountLendingRateTypeGen
  <*> rateStringGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genUri

accountLendingRateTypeGen :: (MonadGen m) => m AccountLendingRateType
accountLendingRateTypeGen = Gen.choice
  [ ALendingRateFixed <$> dateTimeStringGen
  , ALendingRateIntroductory <$> dateTimeStringGen
  , ALendingRateDiscount <$> textGen
  , ALendingRatePenalty <$> textGen
  , ALendingRateBundleDiscount <$> textGen
  , ALendingRateFloating <$> textGen
  , ALendingRateMarketLinked <$> textGen
  , pure ALendingRateCashAdvance
  , pure ALendingRateVariable
  , ALendingRateComparison <$> textGen
  ]


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


transactionGen :: Gen Transaction
transactionGen = Transaction
  <$> accountIdGen
  <*> Gen.maybe transactionIdGen
  <*> Gen.bool
  <*> transactionTypeGen
  <*> transactionStatusGen
  <*> textGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe currencyStringGen
  <*> textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe textGen

transactionIdGen :: Gen TransactionId
transactionIdGen = TransactionId <$> asciiStringGen

transactionTypeGen :: Gen TransactionType
transactionTypeGen = Gen.enumBounded

transactionStatusGen :: Gen TransactionStatus
transactionStatusGen = Gen.choice
  [ pure TransactionStatusPending
  , TransactionStatusPosted <$> dateTimeStringGen
  ]


transactionDetailResponseGen :: Gen TransactionDetailResponse
transactionDetailResponseGen = TransactionDetailResponse
  <$> transactionDetailGen

transactionDetailGen :: Gen TransactionDetail
transactionDetailGen = TransactionDetail
  <$> transactionGen
  <*> transactionExtendedDataGen

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
