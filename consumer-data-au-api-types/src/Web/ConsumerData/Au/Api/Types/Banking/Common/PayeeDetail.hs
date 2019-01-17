{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.PayeeDetail where

import           Control.Monad.Except       (throwError)
import           Country                    (Country)
import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import Country.Waargonaut
    (countryAlphaThreeDecoder, countryAlphaThreeEncoder)
import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Payees
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers
import Web.ConsumerData.Au.Api.Types.Tag


data PayeeDetail = PayeeDetail
  { _payeeDetailPayee :: Payee
  , _payeeTypeData    :: PayeeTypeData
  }
  deriving (Eq, Show)

payeeDetailDecoder :: Monad f => Decoder f PayeeDetail
payeeDetailDecoder =
  PayeeDetail
    <$> payeeDecoder
    <*> payeeTypeDataDecoder
  where
    payeeTypeDataDecoder = typeTaggedDecoder "payeeUType" $ \case
      "domestic" -> Just (TypedTagField PTDDomestic domesticPayeeDecoder)
      "international" -> Just (TypedTagField PTDInternational internationalPayeeDecoder)
      "biller" -> Just (TypedTagField PTDBiller billerPayeeDecoder)
      _ -> Nothing

payeeDetailEncoder :: Applicative f => Encoder f PayeeDetail
payeeDetailEncoder = E.mapLikeObj $ \(PayeeDetail p td) ->
  payeeMLO p .
  payeeFields td
    where
      payeeFields = \case
        PTDDomestic d -> fields "domestic" domesticPayeeEncoder d
        PTDInternational i -> fields "international" internationalPayeeEncoder i
        PTDBiller b -> fields "biller" billerPayeeEncoder b
      fields = typeTaggedField "payeeUType"

instance JsonDecode OB PayeeDetail where
  mkDecoder = tagOb payeeDetailDecoder

instance JsonEncode OB PayeeDetail where
  mkEncoder = tagOb payeeDetailEncoder


data PayeeTypeData
  = PTDDomestic DomesticPayee
  | PTDInternational InternationalPayee
  | PTDBiller BillerPayee
  deriving (Eq, Show)

payeeType :: PayeeTypeData -> PayeeType
payeeType = \case
  PTDDomestic {} -> Domestic
  PTDInternational {} -> International
  PTDBiller {} -> Biller


data DomesticPayee
  = DPAccount DomesticPayeeAccount
  | DPCard DomesticPayeeCard
  | DPPayeeId DomesticPayeePayId
  deriving (Eq, Show)

domesticPayeeDecoder :: Monad f => Decoder f DomesticPayee
domesticPayeeDecoder = typeTaggedDecoder "payeeAccountUType" $ \case
  "account" -> Just (TypedTagField DPAccount domesticPayeeAccountDecoder)
  "card" -> Just (TypedTagField DPCard domesticPayeeCardDecoder)
  "payId" -> Just (TypedTagField DPPayeeId domesticPayeePayIdDecoder)
  _ -> Nothing

domesticPayeeEncoder :: Applicative f => Encoder f DomesticPayee
domesticPayeeEncoder = E.mapLikeObj $ \case
  DPAccount d -> fields "account" domesticPayeeAccountEncoder d
  DPCard c -> fields "card" domesticPayeeCardEncoder c
  DPPayeeId p -> fields "payId" domesticPayeePayIdEncoder p
  where
    fields = typeTaggedField "payeeAccountUType"


data DomesticPayeeAccount
  = DomesticPayeeAccount
  { _domesticPayeeAccountAccountName   :: Text
  , _domesticPayeeAccountBsb           :: Text
  , _domesticPayeeAccountAccountNumber :: Text
  }
  deriving (Eq, Show)

domesticPayeeAccountDecoder :: Monad f => Decoder f DomesticPayeeAccount
domesticPayeeAccountDecoder =
  DomesticPayeeAccount
    <$> D.atKey "accountName" D.text
    <*> D.atKey "bsb" D.text
    <*> D.atKey "accountNumber" D.text

domesticPayeeAccountEncoder :: Applicative f => Encoder f DomesticPayeeAccount
domesticPayeeAccountEncoder = E.mapLikeObj $ \(DomesticPayeeAccount name bsb accNum) ->
  E.atKey' "accountName" E.text name .
  E.atKey' "bsb" E.text bsb .
  E.atKey' "accountNumber" E.text accNum


newtype DomesticPayeeCard
  = DomesticPayeeCard { _domesticPayeeCardCardNumber :: MaskedPanString }
  deriving (Eq, Show)

domesticPayeeCardDecoder :: Monad f => Decoder f DomesticPayeeCard
domesticPayeeCardDecoder =
  DomesticPayeeCard
    <$> D.atKey "cardNumber" maskedPanStringDecoder

domesticPayeeCardEncoder :: Applicative f => Encoder f DomesticPayeeCard
domesticPayeeCardEncoder = E.mapLikeObj $ \(DomesticPayeeCard cn) ->
  E.atKey' "cardNumber" maskedPanStringEncoder cn

-- TODO maybe change this
type MaskedPanString = Text
maskedPanStringDecoder :: Monad f => Decoder f MaskedPanString
maskedPanStringDecoder = D.text
maskedPanStringEncoder :: Applicative f => Encoder f MaskedPanString
maskedPanStringEncoder = E.text


data DomesticPayeePayId
  = DomesticPayeePayId
  { _domesticPayeePayIdTypeName :: Maybe Text
  , _domesticPayeePayIdTypeId   :: Text
  , _domesticPayeePayIdType     :: DomesticPayeePayIdType
  }
  deriving (Eq, Show)

domesticPayeePayIdDecoder :: Monad f => Decoder f DomesticPayeePayId
domesticPayeePayIdDecoder =
  DomesticPayeePayId
    <$> atKeyOptional' "name" D.text
    <*> D.atKey "identifier" D.text
    <*> D.atKey "type" domesticPayeePayIdTypeDecoder

domesticPayeePayIdEncoder :: Applicative f => Encoder f DomesticPayeePayId
domesticPayeePayIdEncoder = E.mapLikeObj $ \(DomesticPayeePayId n i t) ->
  maybeOrAbsentE "name" E.text n .
  E.atKey' "identifier" E.text i .
  E.atKey' "type" domesticPayeePayIdTypeEncoder t


data DomesticPayeePayIdType =
    Email
  | Mobile
  | OrgNumber
  | OrgName
  deriving (Bounded, Enum, Eq, Ord, Show)

domesticPayeePayIdTypeDecoder :: Monad f => Decoder f DomesticPayeePayIdType
domesticPayeePayIdTypeDecoder = D.text >>= \case
  "EMAIL" -> pure Email
  "MOBILE" -> pure Mobile
  "ORG_NUMBER" -> pure OrgNumber
  "ORG_NAME" -> pure OrgName
  s -> throwError (D.ConversionFailure (s <> " is not a valid Domestic Payee ID type"))

domesticPayeePayIdTypeEncoder :: Applicative f => Encoder f DomesticPayeePayIdType
domesticPayeePayIdTypeEncoder = flip contramap E.text $ \case
  Email -> "EMAIL"
  Mobile -> "MOBILE"
  OrgNumber -> "ORG_NUMBER"
  OrgName -> "ORG_NAME"


data InternationalPayee
  = InternationalPayee
  { _internationalPayeeBeneficiaryDetails :: BeneficiaryDetails
  , _internationalPayeeBankDetails        :: BankDetails
  }
  deriving (Eq, Show)

internationalPayeeDecoder :: Monad f => Decoder f InternationalPayee
internationalPayeeDecoder =
  InternationalPayee
    <$> D.atKey "beneficiaryDetails" beneficiaryDetailsDecoder
    <*> D.atKey "bankDetails" bankDetailsDecoder

internationalPayeeEncoder :: Applicative f => Encoder f InternationalPayee
internationalPayeeEncoder = E.mapLikeObj $ \(InternationalPayee bed bad) ->
  E.atKey' "beneficiaryDetails" beneficiaryDetailsEncoder bed .
  E.atKey' "bankDetails" bankDetailsEncoder bad


data BeneficiaryDetails
  = BeneficiaryDetails
  { _beneficiaryDetailsName    :: Maybe Text
  , _beneficiaryDetailsCountry :: Country
  , _beneficiaryDetailsMessage :: Maybe Text
  }
  deriving (Eq, Show)

beneficiaryDetailsDecoder :: Monad f => Decoder f BeneficiaryDetails
beneficiaryDetailsDecoder =
  BeneficiaryDetails
    <$> atKeyOptional' "name" D.text
    <*> D.atKey "country" countryAlphaThreeDecoder
    <*> atKeyOptional' "message" D.text

beneficiaryDetailsEncoder :: Applicative f => Encoder f BeneficiaryDetails
beneficiaryDetailsEncoder = E.mapLikeObj $ \(BeneficiaryDetails n c m) ->
  let f k x = maybeOrAbsentE k E.text x
  in
    f "name" n .
    E.atKey' "country" countryAlphaThreeEncoder c .
    f "message" m


data BankDetails
  = BankDetails
  { _bankDetailsCountry               :: Country
  , _bankDetailsAccountNumber         :: Text
  , _bankDetailsBankAddress           :: Maybe BankAddress
  , _bankDetailsBeneficiaryBankBic    :: Maybe Text
  , _bankDetailsFedWireNumber         :: Maybe Text
  , _bankDetailsSortCode              :: Maybe Text
  , _bankDetailsChipNumber            :: Maybe Text
  , _bankDetailsRoutingNumber         :: Maybe Text
  , _bankDetailsLegalEntityIdentifier :: Maybe Text
  }
  deriving (Eq, Show)

bankDetailsDecoder :: Monad f => Decoder f BankDetails
bankDetailsDecoder =
  let f k = D.atKey k D.text
      fm k = atKeyOptional' k D.text
      bankAddress = atKeyOptional' "bankAddress" bankAddressDecoder
  in  BankDetails
        <$> D.atKey "country" countryAlphaThreeDecoder
        <*> f "accountNumber"
        <*> bankAddress
        <*> fm "beneficiaryBankBIC"
        <*> fm "fedWireNumber"
        <*> fm "sortCode"
        <*> fm "chipNumber"
        <*> fm "routingNumber"
        <*> fm "legalEntityIdentifier"

bankDetailsEncoder :: Applicative f => Encoder f BankDetails
bankDetailsEncoder = E.mapLikeObj $ \(BankDetails c an ba bic fwn sc cn rn lei) ->
  let f k x = E.atKey' k E.text x
      fm k x = maybeOrAbsentE k E.text x
  in
    E.atKey' "country" countryAlphaThreeEncoder c .
    f "accountNumber" an .
    maybeOrAbsentE "bankAddress" bankAddressEncoder ba .
    fm "beneficiaryBankBIC" bic .
    fm "fedWireNumber" fwn .
    fm "sortCode" sc .
    fm "chipNumber" cn .
    fm "routingNumber" rn .
    fm "legalEntityIdentifier" lei


data BankAddress
  = BankAddress
  { _bankAddressName    :: Text
  , _bankAddressAddress :: Text
  }
  deriving (Eq, Show)

bankAddressDecoder :: Monad f => Decoder f BankAddress
bankAddressDecoder =
  BankAddress
    <$> D.atKey "name" D.text
    <*> D.atKey "address" D.text

bankAddressEncoder :: Applicative f => Encoder f BankAddress
bankAddressEncoder = E.mapLikeObj $ \(BankAddress n a) ->
  E.atKey' "name" E.text n .
  E.atKey' "address" E.text a


-- TODO  "crn"  BPay CRN of the Biller. If the contents of the CRN match
-- the format of a Credit Card PAN then it should be masked using the rules
-- applicable for the MaskedPANString common type
data BillerPayee
  = BillerPayee
  { _billerPayeeBillerCode :: Text
  , _billerPayeeCrn        :: Maybe Text
  , _billerPayeeBillerName :: Text
  }
  deriving (Eq, Show)

billerPayeeDecoder :: Monad f => Decoder f BillerPayee
billerPayeeDecoder =
  let f k = D.atKey k D.text
  in BillerPayee
      <$> f "billerCode"
      <*> atKeyOptional' "crn" D.text
      <*> f "billerName"

billerPayeeEncoder :: Applicative f => Encoder f BillerPayee
billerPayeeEncoder = E.mapLikeObj $ \(BillerPayee bc crn bn) ->
  let f k x = E.atKey' k E.text x
  in
    f "billerCode" bc .
    maybeOrAbsentE "crn" E.text crn .
    f "billerName" bn
