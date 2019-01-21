{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDetail
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDetail
  ) where

import           Control.Lens               (Prism', prism, (#))
import           Data.Text                  (Text)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types           (Json, MapLikeObj, WS)

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (Account, AccountId, accountDecoder, accountIdDecoder,
    accountIdEncoder, accountFields)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.DepositRate
    (AccountDepositRates, accountDepositRatesDecoder,
    accountDepositRatesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Feature
    (AccountFeatures, accountFeaturesDecoder, accountFeaturesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Fee
    (AccountFees, accountFeesDecoder, accountFeesEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.LendingRate
    (AccountLendingRates, accountLendingRatesDecoder,
    accountLendingRatesEncoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, CurrencyString, DateString, DurationString,
    amountStringDecoder, amountStringEncoder, currencyStringDecoder,
    currencyStringEncoder, dateStringDecoder, dateStringEncoder,
    durationStringDecoder, durationStringEncoder)
import Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress
    (PhysicalAddress, physicalAddressDecoder, physicalAddressEncoder)
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers
import Web.ConsumerData.Au.Api.Types.Tag


data AccountDetail = AccountDetail
  { _accountDetailAccount         :: Account
  , _accountDetailBsb             :: Maybe Text
  , _accountDetailAccountNumber   :: Maybe Text
  , _accountDetailBundleName      :: Maybe Text
  , _accountDetailSpecificAccount :: Maybe SpecificAccount
  , _accountDetailFeatures        :: Maybe AccountFeatures
  , _accountDetailFees            :: Maybe AccountFees
  , _accountDetailDepositRates    :: Maybe AccountDepositRates
  , _accountDetailLendingRates    :: Maybe AccountLendingRates
  , _accountDetailAddress         :: Maybe PhysicalAddress
  } deriving (Eq, Show)

accountDetailDecoder :: Monad f => Decoder f AccountDetail
accountDetailDecoder =
  AccountDetail
    <$> accountDecoder
    <*> atKeyOptional' "bsb" D.text
    <*> atKeyOptional' "accountNumber" D.text
    <*> atKeyOptional' "bundleName" D.text
    <*> (D.maybeOrNull specificAccountDecoder)
    <*> atKeyOptional' "features" accountFeaturesDecoder
    <*> atKeyOptional' "fees" accountFeesDecoder
    <*> atKeyOptional' "depositRates" accountDepositRatesDecoder
    <*> atKeyOptional' "lendingRates" accountLendingRatesDecoder
    <*> atKeyOptional' "address" physicalAddressDecoder

instance JsonDecode OB AccountDetail where
  mkDecoder = tagOb accountDetailDecoder


accountDetailEncoder :: Applicative f => Encoder f AccountDetail
accountDetailEncoder = E.mapLikeObj $ \p ->
  accountFields (_accountDetailAccount p) .
  maybeOrAbsentE "bsb" E.text (_accountDetailBsb p) .
  maybeOrAbsentE "accountNumber" E.text (_accountDetailAccountNumber p) .
  maybeOrAbsentE "bundleName" E.text (_accountDetailBundleName p) .
  maybe id specificAccountFields (_accountDetailSpecificAccount p) .
  maybeOrAbsentE "features" accountFeaturesEncoder (_accountDetailFeatures p) .
  maybeOrAbsentE "fees" accountFeesEncoder (_accountDetailFees p) .
  maybeOrAbsentE "depositRates" accountDepositRatesEncoder (_accountDetailDepositRates p) .
  maybeOrAbsentE "lendingRates" accountLendingRatesEncoder (_accountDetailLendingRates p) .
  maybeOrAbsentE "address" physicalAddressEncoder (_accountDetailAddress p)


instance JsonEncode OB AccountDetail where
  mkEncoder = tagOb accountDetailEncoder


data SpecificAccount =
    TermDeposit TermDepositAccountType
  | CreditCard CreditCardAccountType
  | Loan LoanAccountType
  deriving (Eq, Show)

specificAccountDecoder :: Monad f => Decoder f SpecificAccount
specificAccountDecoder = typeTaggedDecoder "specificAccountUType" $ \case
    "termDeposit" -> Just $ (TypedTagField TermDeposit termDepositAccountTypeDecoder)
    "creditCard"  -> Just $ (TypedTagField CreditCard creditCardAccountTypeDecoder)
    "loan"        -> Just $ (TypedTagField Loan loanAccountTypeDecoder)
    _             -> Nothing

instance JsonDecode OB SpecificAccount where
  mkDecoder = tagOb specificAccountDecoder

specificAccountEncoder :: Applicative f => Encoder f SpecificAccount
specificAccountEncoder = E.mapLikeObj specificAccountFields

specificAccountFields :: SpecificAccount -> MapLikeObj WS Json -> MapLikeObj WS Json
specificAccountFields = \case
  TermDeposit t -> fields "termDeposit" termDepositAccountTypeEncoder t
  CreditCard c -> fields "creditCard" creditCardAccountTypeEncoder c
  Loan l -> fields "loan" loanAccountTypeEncoder l
  where
    fields = typeTaggedField "specificAccountUType"

instance JsonEncode OB SpecificAccount where
  mkEncoder = tagOb specificAccountEncoder


data TermDepositAccountType = TermDepositAccountType
  { _termDepositAccountTypeLodgementDate        :: DateString
  , _termDepositAccountTypeMaturityDate         :: DateString
  , _termDepositAccountTypeMaturityAmount       :: Maybe AmountString
  , _termDepositAccountTypeMaturityCurrency     :: Maybe CurrencyString
  , _termDepositAccountTypeMaturityInstructions :: MaturityInstructions
  } deriving (Eq, Show)

termDepositAccountTypeDecoder :: Monad f => Decoder f TermDepositAccountType
termDepositAccountTypeDecoder =
  TermDepositAccountType
    <$> D.atKey "lodgementDate" dateStringDecoder
    <*> D.atKey "maturityDate" dateStringDecoder
    <*> atKeyOptional' "maturityAmount" amountStringDecoder
    <*> atKeyOptional' "maturityCurrency" currencyStringDecoder
    <*> D.atKey "maturityInstructions" maturityInstructionsDecoder

instance JsonDecode OB TermDepositAccountType where
  mkDecoder = tagOb termDepositAccountTypeDecoder

termDepositAccountTypeEncoder :: Applicative f => Encoder f TermDepositAccountType
termDepositAccountTypeEncoder = E.mapLikeObj $ \p ->
    E.atKey' "lodgementDate" dateStringEncoder (_termDepositAccountTypeLodgementDate p) .
    E.atKey' "maturityDate" dateStringEncoder (_termDepositAccountTypeMaturityDate p) .
    maybeOrAbsentE "maturityAmount" amountStringEncoder (_termDepositAccountTypeMaturityAmount p) .
    maybeOrAbsentE "maturityCurrency" currencyStringEncoder (_termDepositAccountTypeMaturityCurrency p) .
    E.atKey' "maturityInstructions" maturityInstructionsEncoder (_termDepositAccountTypeMaturityInstructions p)

instance JsonEncode OB TermDepositAccountType where
  mkEncoder = tagOb termDepositAccountTypeEncoder


data MaturityInstructions =
    MaturityInstructionsRolledOver -- ^ "ROLLED_OVER"
  | MaturityInstructionsPaidOutAtMaturity -- ^ "PAID_OUT_AT_MATURITY"
  deriving (Bounded, Enum, Eq, Ord, Show)

maturityInstructionsText ::
  Prism' Text MaturityInstructions
maturityInstructionsText =
  prism (\case
          MaturityInstructionsRolledOver -> "ROLLED_OVER"
          MaturityInstructionsPaidOutAtMaturity -> "PAID_OUT_AT_MATURITY"
      )
      (\case
          "ROLLED_OVER" -> Right MaturityInstructionsRolledOver
          "PAID_OUT_AT_MATURITY" -> Right MaturityInstructionsPaidOutAtMaturity
          t -> Left t
      )

maturityInstructionsDecoder :: Monad m =>
  D.Decoder m MaturityInstructions
maturityInstructionsDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid MaturityInstructions")
  maturityInstructionsText
  D.text

maturityInstructionsEncoder ::
  E.Encoder' MaturityInstructions
maturityInstructionsEncoder =
  E.prismE maturityInstructionsText E.text'


data CreditCardAccountType = CreditCardAccountType
  { _creditCardAccountTypeMinPaymentAmount :: AmountString
  , _creditCardAccountTypePaymentDueAmount :: AmountString
  , _creditCardAccountTypePaymentCurrency  :: Maybe CurrencyString
  , _creditCardAccountTypePaymentDueDate   :: DateString
  } deriving (Eq, Show)

creditCardAccountTypeDecoder :: Monad f => Decoder f CreditCardAccountType
creditCardAccountTypeDecoder =
  CreditCardAccountType
    <$> D.atKey "minPaymentAmount" amountStringDecoder
    <*> D.atKey "paymentDueAmount" amountStringDecoder
    <*> atKeyOptional' "paymentCurrency" currencyStringDecoder
    <*> D.atKey "paymentDueDate" dateStringDecoder

instance JsonDecode OB CreditCardAccountType where
  mkDecoder = tagOb creditCardAccountTypeDecoder

creditCardAccountTypeEncoder :: Applicative f => Encoder f CreditCardAccountType
creditCardAccountTypeEncoder = E.mapLikeObj $ \p ->
    E.atKey' "minPaymentAmount" amountStringEncoder (_creditCardAccountTypeMinPaymentAmount p) .
    E.atKey' "paymentDueAmount" amountStringEncoder (_creditCardAccountTypePaymentDueAmount p) .
    maybeOrAbsentE "paymentCurrency" currencyStringEncoder (_creditCardAccountTypePaymentCurrency p) .
    E.atKey' "paymentDueDate" dateStringEncoder (_creditCardAccountTypePaymentDueDate p)

instance JsonEncode OB CreditCardAccountType where
  mkEncoder = tagOb creditCardAccountTypeEncoder


data LoanAccountType = LoanAccountType
  { _loanAccountTypeOriginalStartDate     :: Maybe DateString
  , _loanAccountTypeOriginalLoanAmount    :: Maybe AmountString
  , _loanAccountTypeOriginalLoanCurrency  :: Maybe CurrencyString
  , _loanAccountTypeLoanEndDate           :: Maybe DateString
  , _loanAccountTypeNextInstalmentDate    :: Maybe DateString
  , _loanAccountTypeMinInstalmentAmount   :: Maybe AmountString
  , _loanAccountTypeMinInstalmentCurrency :: Maybe CurrencyString
  , _loanAccountTypeMaxRedraw             :: Maybe AmountString
  , _loanAccountTypeMaxRedrawCurrency     :: Maybe CurrencyString
  , _loanAccountTypeMinRedraw             :: Maybe AmountString
  , _loanAccountTypeMinRedrawCurrency     :: Maybe CurrencyString
  , _loanAccountTypeOffsetAccountEnabled  :: Maybe Bool
  , _loanAccountTypeOffsetAccountIds      :: Maybe [AccountId]
  , _loanAccountTypeRepaymentFrequency    :: Maybe DurationString
  , _loanAccountTypeRepaymentType         :: Maybe RepaymentType
  } deriving (Eq, Show)

loanAccountTypeDecoder :: Monad f => Decoder f LoanAccountType
loanAccountTypeDecoder =
  LoanAccountType
    <$> atKeyOptional' "originalStartDate" dateStringDecoder
    <*> atKeyOptional' "originalLoanAmount" amountStringDecoder
    <*> atKeyOptional' "originalLoanCurrency" currencyStringDecoder
    <*> atKeyOptional' "loanEndDate" dateStringDecoder
    <*> atKeyOptional' "nextInstalmentDate" dateStringDecoder
    <*> atKeyOptional' "minInstalmentAmount" amountStringDecoder
    <*> atKeyOptional' "minInstalmentCurrency" currencyStringDecoder
    <*> atKeyOptional' "maxRedraw" amountStringDecoder
    <*> atKeyOptional' "maxRedrawCurrency" currencyStringDecoder
    <*> atKeyOptional' "minRedraw" amountStringDecoder
    <*> atKeyOptional' "minRedrawCurrency" currencyStringDecoder
    <*> atKeyOptional' "offsetAccountEnabled" D.bool
    <*> atKeyOptional' "offsetAccountIds" (D.list accountIdDecoder)
    <*> atKeyOptional' "repaymentFrequency" durationStringDecoder
    <*> atKeyOptional' "repaymentType" repaymentTypeDecoder

instance JsonDecode OB LoanAccountType where
  mkDecoder = tagOb loanAccountTypeDecoder

loanAccountTypeEncoder :: Applicative f => Encoder f LoanAccountType
loanAccountTypeEncoder = E.mapLikeObj $ \p ->
  maybeOrAbsentE "originalStartDate" dateStringEncoder (_loanAccountTypeOriginalStartDate p) .
  maybeOrAbsentE "originalLoanAmount" amountStringEncoder (_loanAccountTypeOriginalLoanAmount p) .
  maybeOrAbsentE "originalLoanCurrency" currencyStringEncoder (_loanAccountTypeOriginalLoanCurrency p) .
  maybeOrAbsentE "loanEndDate" dateStringEncoder (_loanAccountTypeLoanEndDate p) .
  maybeOrAbsentE "nextInstalmentDate" dateStringEncoder (_loanAccountTypeNextInstalmentDate p) .
  maybeOrAbsentE "minInstalmentAmount" amountStringEncoder (_loanAccountTypeMinInstalmentAmount p) .
  maybeOrAbsentE "minInstalmentCurrency" currencyStringEncoder (_loanAccountTypeMinInstalmentCurrency p) .
  maybeOrAbsentE "maxRedraw" amountStringEncoder (_loanAccountTypeMaxRedraw p) .
  maybeOrAbsentE "maxRedrawCurrency" currencyStringEncoder (_loanAccountTypeMaxRedrawCurrency p) .
  maybeOrAbsentE "minRedraw" amountStringEncoder (_loanAccountTypeMinRedraw p) .
  maybeOrAbsentE "minRedrawCurrency" currencyStringEncoder (_loanAccountTypeMinRedrawCurrency p) .
  maybeOrAbsentE "offsetAccountEnabled" E.bool (_loanAccountTypeOffsetAccountEnabled p) .
  maybeOrAbsentE "offsetAccountIds" (E.list accountIdEncoder) (_loanAccountTypeOffsetAccountIds p) .
  maybeOrAbsentE "repaymentFrequency" durationStringEncoder (_loanAccountTypeRepaymentFrequency p) .
  maybeOrAbsentE "repaymentType" repaymentTypeEncoder (_loanAccountTypeRepaymentType p)

instance JsonEncode OB LoanAccountType where
  mkEncoder = tagOb loanAccountTypeEncoder


data RepaymentType =
    RepaymentTypeInterestOnly -- ^ "INTEREST_ONLY"
  | RepaymentTypePrincipalAndInterest -- ^ "PRINCIPAL_AND_INTEREST"
  deriving (Bounded, Enum, Eq, Ord, Show)

repaymentTypeText ::
  Prism' Text RepaymentType
repaymentTypeText =
  prism (\case
          RepaymentTypeInterestOnly -> "INTEREST_ONLY"
          RepaymentTypePrincipalAndInterest -> "PRINCIPAL_AND_INTEREST"
      )
      (\case
          "INTEREST_ONLY" -> Right RepaymentTypeInterestOnly
          "PRINCIPAL_AND_INTEREST" -> Right RepaymentTypePrincipalAndInterest
          t -> Left t
      )

repaymentTypeDecoder :: Monad m =>
  D.Decoder m RepaymentType
repaymentTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid RepaymentType")
  repaymentTypeText
  D.text

repaymentTypeEncoder ::
  E.Encoder' RepaymentType
repaymentTypeEncoder =
  E.prismE repaymentTypeText E.text'


-- | TODO The expected or required repayment frequency. Formatted according to <https://en.wikipedia.org/wiki/ISO_8601#Durations ISO 8601 Durations>
data RepaymentFrequency =
  RepaymentFrequency Text
  deriving (Eq, Show)
