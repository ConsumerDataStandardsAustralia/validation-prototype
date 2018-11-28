module Web.ConsumerData.Au.Api.Types.Data.Gens where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import Country.Gens (countryGen)
import Data.Text.Gens (textGen)

import Web.ConsumerData.Au.Api.Types.Data.Currency
import Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress

currencyGen :: Gen Currency
currencyGen = Gen.enumBounded

physicalAddressGen ::  Gen PhysicalAddress
physicalAddressGen =
  PhysicalAddress
    <$> addressPurposeGen
    <*> addressGen

addressPurposeGen :: Gen AddressPurpose
addressPurposeGen = Gen.element
  [ AddressPurposeRegistered
  , AddressPurposeMail
  , AddressPurposePhysical
  , AddressPurposeWork
  , AddressPurposeOther
  ]

addressGen :: Gen Address
addressGen =  Gen.choice
  [ AddressSimple <$> simpleAddressGen
  , pure AddressPaf
  ]

simpleAddressGen :: Gen SimpleAddress
simpleAddressGen =
  SimpleAddress
    <$> Gen.maybe textGen
    <*> textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> Gen.maybe textGen
    <*> textGen
    <*> addressStateGen
    <*> Gen.maybe countryGen

addressStateGen :: Gen AddressState
addressStateGen = Gen.choice
  [ AustralianState <$> australiaStateGen
  , OtherCountryState <$> textGen
  ]

australiaStateGen :: Gen AustraliaState
australiaStateGen = Gen.element
  [ AustraliaStateACT
  , AustraliaStateNSW
  , AustraliaStateNT
  , AustraliaStateQLD
  , AustraliaStateSA
  , AustraliaStateTAS
  , AustraliaStateVIC
  , AustraliaStateWA
  ]