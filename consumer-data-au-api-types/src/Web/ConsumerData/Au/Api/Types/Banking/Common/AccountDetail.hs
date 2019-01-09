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



-- AccountDetail <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaaccountdetail CDR AU v0.1.0 AccountDetail>
data AccountDetail = AccountDetail
  { _accountDetailAccount         :: Account
  , _accountDetailBsb             :: Maybe Text
  , _accountDetailAccountNumber   :: Maybe Text
  , _accountDetailBundleName      :: Maybe Text -- ^ Indicates if this account is park of a bundle that is providing additional benefit to the customer.
  , _accountDetailSpecificAccount :: Maybe SpecificAccount -- ^ Account specific fields.
  , _accountDetailFeatures        :: Maybe AccountFeatures -- ^ Array of features on the account
  , _accountDetailFees            :: Maybe AccountFees -- ^ Fees and charges applicable to the account
  , _accountDetailDepositRates    :: Maybe AccountDepositRates -- ^ Interest rates available for deposits
  , _accountDetailLendingRates    :: Maybe AccountLendingRates -- ^ Interest rates charged against lending balances
  , _accountDetailAddress         :: Maybe PhysicalAddress -- ^ The address for the account to be used for correspondence
  -- WARNING
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


-- | TermDepositAccountType <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schematermdepositaccounttype CDR AU v0.1.0 TermDepositAccountType>
data TermDepositAccountType = TermDepositAccountType
  { _termDepositAccountTypeLodgementDate        :: DateString -- ^ The lodgement date of the original deposit.
  , _termDepositAccountTypeMaturityDate         :: DateString -- ^ Maturity date for the term deposit.
  , _termDepositAccountTypeMaturityAmount       :: Maybe AmountString -- ^ WARNING number in standard (7/11/18) Amount to be paid upon maturity. If absent it implies the amount to paid is variable and cannot currently be calculated
  , _termDepositAccountTypeMaturityCurrency     :: Maybe CurrencyString -- ^ If absent assumes AUD.
  , _termDepositAccountTypeMaturityInstructions :: MaturityInstructions -- ^ Current instructions on action to be taken at maturity.
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


-- | CreditCardAccountType <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemacreditcardaccounttype CDR AU v0.1.0 CreditCardAccountType>
data CreditCardAccountType = CreditCardAccountType
  { _creditCardAccountTypeMinPaymentAmount :: AmountString -- ^ The minimum payment amount due for the next card payment.
  , _creditCardAccountTypePaymentDueAmount :: AmountString -- ^ The amount due for the next card payment.
  , _creditCardAccountTypePaymentCurrency  :: Maybe CurrencyString -- ^ If absent assumes AUD.
  , _creditCardAccountTypePaymentDueDate   :: DateString -- ^ Date that the next payment for the card is due.
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


-- | LoanAccountType <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaloanaccounttype CDR AU v0.1.0 LoanAccountType>
data LoanAccountType = LoanAccountType
  { _loanAccountTypeOriginalStartDate     :: Maybe DateString -- ^ Optional original start date for the loan.
  , _loanAccountTypeOriginalLoanAmount    :: Maybe AmountString -- ^ Optional original loan value.
  , _loanAccountTypeOriginalLoanCurrency  :: Maybe CurrencyString -- ^ If absent assumes AUD.
  , _loanAccountTypeLoanEndDate           :: Maybe DateString -- ^ Date that the loan is due to be repaid in full.
  , _loanAccountTypeNextInstalmentDate    :: Maybe DateString -- ^ Next date that an installment is required.
  , _loanAccountTypeMinInstalmentAmount   :: Maybe AmountString -- ^ Minimum Amount of next instalment.
  , _loanAccountTypeMinInstalmentCurrency :: Maybe CurrencyString -- ^ If absent assumes AUD.
  , _loanAccountTypeMaxRedraw             :: Maybe AmountString -- ^ Maximum amount of funds that can be redrawn. If not present redraw is not available even if the feature exists for the account.
  , _loanAccountTypeMaxRedrawCurrency     :: Maybe CurrencyString -- ^ If absent assumes AUD.
  , _loanAccountTypeMinRedraw             :: Maybe AmountString -- ^ Minimum redraw amount.
  , _loanAccountTypeMinRedrawCurrency     :: Maybe CurrencyString -- ^ If absent assumes AUD.
  , _loanAccountTypeOffsetAccountEnabled  :: Maybe Bool -- ^ Set to true if one or more offset accounts are configured for this loan account
  , _loanAccountTypeOffsetAccountIds      :: Maybe [AccountId] -- ^ The accountIDs of the configured offset accounts attached to this loan. Only offset accounts that can be accesses under the current authorisation should be included. It is expected behaviour that offsetAccountEnabled is set to true but the offsetAccountIds field is absent or empty. This represents a situation where an offset account exists but details can not be accessed under the current authorisation.
  , _loanAccountTypeRepaymentFrequency    :: Maybe DurationString -- ^ The expected or required repayment frequency. Formatted according to <https://en.wikipedia.org/wiki/ISO_8601#Durations ISO 8601 Durations>
  , _loanAccountTypeRepaymentType         :: Maybe RepaymentType -- ^ Options in place for repayments. If absent defaults to PRINCIPAL_AND_INTEREST.

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


-- | The expected or required repayment frequency. Formatted according to <https://en.wikipedia.org/wiki/ISO_8601#Durations ISO 8601 Durations>
data RepaymentFrequency =
  RepaymentFrequency Text
  deriving (Eq, Show)
