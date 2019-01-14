module Web.ConsumerData.Au.Api.Types.Common.Gens where

import           Hedgehog       (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Country.Gens   (countryGen)
import Data.Text.Gens (textGen)
import Data.Time.Gens (utcTimeGen)

import Data.Vector.V5
import Data.Vector.V6
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
  Person
    <$> utcTimeGen
    <*> Gen.maybe textGen
    <*> textGen
    <*> Gen.list (Range.linear 0 2) textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe occupationCodeGen

occupationCodeGen :: Gen OccupationCode
occupationCodeGen = OccupationCode <$> v6Gen Gen.enumBounded

v6Gen :: Gen a -> Gen (V6 a)
v6Gen a = sequenceA (V6 a a a a a a)

v5Gen :: Gen a -> Gen (V5 a)
v5Gen a = sequenceA (V5 a a a a a)

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
    <*> Gen.maybe industryCodeGen
    <*> organisationTypeGen
    <*> Gen.maybe countryGen
    <*> Gen.maybe utcTimeGen

industryCodeGen :: Gen IndustryCode
industryCodeGen = IndustryCode <$> v5Gen Gen.enumBounded

organisationTypeGen :: Gen OrganisationType
organisationTypeGen = Gen.enumBounded


personDetailGen :: Gen PersonDetail
personDetailGen =
  PersonDetail
    <$> personGen
    <*> Gen.nonEmpty (Range.linear 1 3) phoneNumberGen
    <*> Gen.list (Range.linear 0 3) emailAddressGen
    <*> Gen.nonEmpty (Range.linear 1 3) physicalAddressWithPurposeGen

organisationDetailGen :: Gen OrganisationDetail
organisationDetailGen =
  OrganisationDetail
    <$> organisationGen
    <*> Gen.nonEmpty (Range.linear 1 3) physicalAddressWithPurposeGen

emailAddressGen :: Gen EmailAddress
emailAddressGen = EmailAddress <$> Gen.bool <*> emailAddressPurposeGen <*> textGen

emailAddressPurposeGen :: Gen EmailAddressPurpose
emailAddressPurposeGen = Gen.enumBounded

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
phoneNumberPurposeGen = Gen.enumBounded
