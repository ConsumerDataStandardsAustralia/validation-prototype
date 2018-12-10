{-# LANGUAGE RankNTypes #-}

module Web.ConsumerData.Au.Api.Types.Banking.ProductsGens where

import           Control.Monad.Catch (MonadThrow)
import           Hedgehog            (MonadGen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Text.URI.Gens       (genURI)

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Constraint
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.DepositRate
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Discount
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Eligibility
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Feature
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Fee
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.LendingRate
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens


productGen :: forall m. (MonadGen m, MonadThrow m) => m Product
productGen  = Product
  <$> asciiStringGen
  <*> Gen.maybe dateTimeStringGen
  <*> Gen.maybe dateTimeStringGen
  <*> dateTimeStringGen
  <*> productCategoryGen
  <*> textGen
  <*> textGen
  <*> textGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genURI
  <*> Gen.bool_
  <*> Gen.maybe productAdditionalInformationGen

productAdditionalInformationGen :: (MonadGen m, MonadThrow m) => m ProductAdditionalInformation
productAdditionalInformationGen = ProductAdditionalInformation
  <$> Gen.maybe genURI
  <*> Gen.maybe genURI
  <*> Gen.maybe genURI
  <*> Gen.maybe genURI
  <*> Gen.maybe genURI

productCategoryGen :: (MonadGen m) => m ProductCategory
productCategoryGen = Gen.element
  [ PCPersAtCallDeposits
  , PCBusAtCallDeposits
  , PCTermDeposits
  , PCResidential_mortgages
  , PCPersCredAndChrgCards
  , PCBusCredAndChrgCards
  , PCPersLoans
  , PCPersLeasing
  , PCBusLeasing
  , PCTradeFinance
  , PCPersOverdraft
  , PCBusOverdraft
  , PCBusLoans
  , PCForeignCurrAtCallDeposits
  , PCForeignCurrTermDeposits
  , PCForeignCurrLoan
  , PCForeignCurrrenctOverdraft
  , PCTravelCard
  ]


productDetailGen :: (MonadGen m, MonadThrow m) => m ProductDetail
productDetailGen = ProductDetail
  <$> productGen
  <*> Gen.maybe productBundlesGen
  <*> Gen.maybe productFeaturesGen
  <*> Gen.maybe productConstraintsGen
  <*> Gen.maybe productEligibilitiesGen
  <*> Gen.maybe productFeesGen
  <*> Gen.maybe productDepositRatesGen
  <*> Gen.maybe productLendingRatesGen
  <*> Gen.maybe productRepaymentTypeGen

productBundlesGen :: (MonadGen m, MonadThrow m) => m ProductBundles
productBundlesGen = ProductBundles
  <$> Gen.list (Range.linear 0 10) productBundleGen

productBundleGen :: (MonadGen m, MonadThrow m) => m ProductBundle
productBundleGen = ProductBundle
  <$> textGen
  <*> textGen
  <*> Gen.maybe genURI
  <*> Gen.list (Range.linear 0 10) textGen

productRepaymentTypeGen :: (MonadGen m) => m ProductRepaymentType
productRepaymentTypeGen = Gen.element
  [ PRepaymentTypeInterestOnly
  , PRepaymentTypePrincipalAndInterest
  , PRepaymentTypeNegotiable
  ]


productConstraintsGen :: (MonadGen m) => m ProductConstraints
productConstraintsGen = ProductConstraints
  <$> Gen.list (Range.linear 0 10) productConstraintGen

productConstraintGen :: (MonadGen m) => m ProductConstraint
productConstraintGen = Gen.lift $ Gen.choice
  [ PConstraintMinBalance <$> amountStringGen
  , PConstraintOpeningBalance <$> amountStringGen
  , PConstraintMaxLimit <$> amountStringGen
  , PConstraintMinLimit <$> amountStringGen
  ]


productDepositRatesGen :: (MonadGen m, MonadThrow m) => m ProductDepositRates
productDepositRatesGen = ProductDepositRates
  <$> Gen.list (Range.linear 0 10) productDepositRateGen

productDepositRateGen :: (MonadGen m, MonadThrow m) => m ProductDepositRate
productDepositRateGen = ProductDepositRate
  <$> productDepositRateTypeGen
-- WARNING
  <*> rateStringGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genURI

productDepositRateTypeGen :: (MonadGen m) => m ProductDepositRateType
productDepositRateTypeGen = Gen.lift $ Gen.choice
  [ PDepositRateTypeFixed <$> durationStringGen
  , PDepositRateTypeBonus <$> textGen
  , PDepositRateTypeBundleBonus <$> textGen
  , pure PDepositRateTypeVariable
  , PDepositRateTypeIntroductory <$> durationStringGen
  ]


productDiscountsGen :: (MonadGen m) => m ProductDiscounts
productDiscountsGen = ProductDiscounts
  <$> Gen.list (Range.linear 0 10) productDiscountGen

productDiscountGen :: (MonadGen m) => m ProductDiscount
productDiscountGen = ProductDiscount
  <$> textGen
  <*> productDiscountTypeGen
-- WARNING
  <*> amountStringGen

productDiscountTypeGen :: (MonadGen m) => m ProductDiscountType
productDiscountTypeGen = Gen.lift $ Gen.choice
  [ PDiscountBalance <$> amountStringGen
  , PDiscountDeposits <$> amountStringGen
  , PDiscountPayments <$> amountStringGen
  , PDiscountBundle <$> textGen
  ]


productEligibilitiesGen :: (MonadGen m, MonadThrow m) => m ProductEligibilities
productEligibilitiesGen = ProductEligibilities
  <$> Gen.list (Range.linear 0 10) productEligibilityGen

productEligibilityGen :: (MonadGen m, MonadThrow m) => m ProductEligibility
productEligibilityGen = ProductEligibility
  <$> textGen
  <*> productEligibilityTypeGen
-- WARNING
  <*> Gen.maybe textGen
  <*> Gen.maybe genURI

productEligibilityTypeGen :: (MonadGen m) => m ProductEligibilityType
productEligibilityTypeGen = Gen.lift $ Gen.choice
  [ pure PEligibilityBusiness
  , pure PEligibilityPensionRecipient
  , PEligibilityMinAge <$> intGen
  , PEligibilityMaxAge <$> intGen
  , PEligibilityMinIncome <$> amountStringGen
  , PEligibilityMinTurnover <$> amountStringGen
  , pure PEligibilityStaff
  , pure PEligibilityStudent
  , PEligibilityEmploymentStatus <$> textGen
  , PEligibilityResidencyStatus <$> textGen
  , PEligibilityOther <$> textGen
  ]


productFeaturesGen :: (MonadGen m) => m ProductFeatures
productFeaturesGen = ProductFeatures
  <$> Gen.list (Range.linear 0 10) productFeatureTypeGen

productFeatureTypeGen :: (MonadGen m) => m ProductFeatureType
productFeatureTypeGen = Gen.lift $ Gen.choice
  [ pure PFeatureCardAcess
  , PFeatureAdditionalCards <$> intGen
  , pure PFeatureUnlimitedTxns
  , PFeatureFreeTxns <$> intGen
  , PFeatureFreeTxnsAllowance <$> amountStringGen
  , PFeatureLoyaltyProgram <$> textGen
  , pure PFeatureOffset
  , pure PFeatureOverdraft
  , pure PFeatureRedraw
  , PFeatureInsurance <$> textGen
  , pure PFeatureBalanceTransfers
  , PFeatureInterestFree <$> durationStringGen
  , PFeatureInterestFreeTransfers <$> durationStringGen
  , PFeatureDigitalWallet <$> textGen
  , pure PFeatureDigitalBanking
  , pure PFeatureNppPayid
  , pure PFeatureNppEnabled
  , pure PFeatureDonateInterest
  , PFeatureBillPayment <$> textGen
  ]


productFeesGen :: (MonadGen m, MonadThrow m) => m ProductFees
productFeesGen = ProductFees
  <$> Gen.list (Range.linear 0 10) productFeeGen

productFeeGen :: (MonadGen m, MonadThrow m) => m ProductFee
productFeeGen = ProductFee
  <$> textGen
  <*> productFeeTypeGen
-- WARNING
  <*> Gen.maybe amountStringGen
  <*> Gen.maybe rateStringGen
  <*> Gen.maybe rateStringGen
  <*> Gen.maybe currencyStringGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genURI
  <*> Gen.maybe productDiscountsGen

productFeeTypeGen :: (MonadGen m) => m ProductFeeType
productFeeTypeGen = Gen.lift $ Gen.choice
  [ PFeePeriodicPeriodic <$> durationStringGen
  , PFeePeriodicTransaction <$> textGen
  , pure PFeePeriodicEstablishment
  , pure PFeePeriodicExit
  , pure PFeePeriodicOverdraw
  , PFeePeriodicMinBalance <$> durationStringGen
  , pure PFeePeriodicRedraw
  , pure PFeePeriodicChequeCash
  , pure PFeePeriodicChequeStop
  , pure PFeePeriodicChequeBook
  , pure PFeePeriodicCardReplace
  , pure PFeePeriodicPaperStatement
  , PFeePeriodicOtherEvent <$> textGen
  ]


productLendingRatesGen :: (MonadGen m, MonadThrow m) => m ProductLendingRates
productLendingRatesGen = ProductLendingRates
  <$> Gen.list (Range.linear 0 10) productLendingRateGen

productLendingRateGen :: (MonadGen m, MonadThrow m) => m ProductLendingRate
productLendingRateGen = ProductLendingRate
  <$> productLendingRateTypeGen
-- WARNING
  <*> rateStringGen
  <*> Gen.maybe textGen
  <*> Gen.maybe genURI

productLendingRateTypeGen :: (MonadGen m) => m ProductLendingRateType
productLendingRateTypeGen = Gen.lift $ Gen.choice
  [ PLendingRateFixed <$> durationStringGen
  , PLendingRateIntroductory <$> durationStringGen
  , PLendingRateDiscount <$> textGen
  , PLendingRatePenalty <$> textGen
  , PLendingRateBundleDiscount <$> textGen
  , PLendingRateFloating <$> textGen
  , PLendingRateMarketLinked <$> textGen
  , pure PLendingRateCashAdvance
  , pure PLendingRateVariable
  , PLendingRateComparison <$> textGen
  ]
