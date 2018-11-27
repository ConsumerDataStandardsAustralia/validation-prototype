module Web.ConsumerData.Au.Api.Types.Common.Gens where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Country.Gens (countryGen)
import Data.Text.Gens (textGen)
import Data.Time.Gens (utcTimeGen)

import Web.ConsumerData.Au.Api.Types.Common.Customer
import Web.ConsumerData.Au.Api.Types.Data.Gens

customerResponseGen :: Gen CustomerResponse
customerResponseGen = Gen.choice
  [ CustomerPerson <$> personGen
  , CustomerOrganisation <$> organisationGen
  ]

customerDetailResponseGen :: Gen CustomerDetailResponse
customerDetailResponseGen = Gen.choice
  [ CustomerDetailPerson <$> personDetailGen
  , CustomerDetailOrganisation <$> organisationDetailGen
  ]

personGen :: Gen Person
personGen =
  Person <$> utcTimeGen <*> textGen <*> textGen <*> Gen.list (Range.linear 0 2) textGen
  <*> textGen <*> Gen.maybe textGen <*> Gen.maybe occupationCodeGen

occupationCodeGen :: Gen OccupationCode
occupationCodeGen = OccupationCode <$> textGen

organisationGen :: Gen Organisation
organisationGen =
  Organisation 
    <$> utcTimeGen
    <*> Gen.maybe textGen
    <*> textGen
    <*> textGen
    <*> textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe Gen.bool
    <*> Gen.maybe textGen
    <*> Gen.maybe organisationTypeGen
    <*> Gen.maybe countryGen
    <*> Gen.maybe utcTimeGen

organisationTypeGen :: Gen OrganisationType
organisationTypeGen = Gen.element
  [ OrgTypeSoleTrader
  , OrgTypeCompany
  , OrgTypePartnership
  , OrgTypeTrust
  , OrgTypeGovermentEntity
  , OrgTypeOther
  ]

personDetailGen :: Gen PersonDetail
personDetailGen =
  PersonDetail
    <$> personGen
    <*> Gen.nonEmpty (Range.linear 1 3) phoneNumberGen
    <*> Gen.list (Range.linear 0 3) emailAddressGen
    <*> Gen.list (Range.linear 0 3) physicalAddressGen

organisationDetailGen :: Gen OrganisationDetail
organisationDetailGen =
  OrganisationDetail
    <$> organisationGen
    <*> Gen.list (Range.linear 0 3) physicalAddressGen

emailAddressGen :: Gen EmailAddress
emailAddressGen = EmailAddress <$> Gen.bool <*> emailAddressPurposeGen <*> textGen

emailAddressPurposeGen :: Gen EmailAddressPurpose
emailAddressPurposeGen = Gen.element
  [ EmailAddressPurposeWork
  , EmailAddressPurposeHome
  , EmailAddressPurposeOther
  , EmailAddressPurposeUnspecified
  ]

phoneNumberGen :: Gen PhoneNumber
phoneNumberGen =
  PhoneNumber
    <$> Gen.bool
    <*> phoneNumberPurposeGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> textGen
    <*> Gen.maybe textGen
    <*> textGen

phoneNumberPurposeGen :: Gen PhoneNumberPurpose
phoneNumberPurposeGen = Gen.element
  [ PhoneNumberPurposeMobile
  , PhoneNumberPurposeWork
  , PhoneNumberPurposeHome
  , PhoneNumberPurposeOther
  , PhoneNumberPurposeInternational
  , PhoneNumberPurposeUnspecified
  ]
