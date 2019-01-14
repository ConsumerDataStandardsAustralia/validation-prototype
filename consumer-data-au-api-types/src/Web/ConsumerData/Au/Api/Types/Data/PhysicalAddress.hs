{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress
  ( module Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress
  ) where

import           Control.Lens               (Prism', prism, ( # ))
import           Country                    (Country)
import           Country.Waargonaut
    (countryAlphaThreeDecoder, countryAlphaThreeEncoder)
import           Data.Functor.Alt           ((<!>))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Stub
    (emptyObjDecoder, emptyObjEncoder)
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers


data PhysicalAddressWithPurpose = PhysicalAddressWithPurpose
  { _physicalAddressWPPurpose :: AddressPurpose
  , _physicalAddressWPAddress :: Address
  } deriving (Generic, Show, Eq)

physicalAddressWithPurposeEncoder :: Applicative f => Encoder f PhysicalAddressWithPurpose
physicalAddressWithPurposeEncoder = E.mapLikeObj $ \pa ->
  (E.atKey' "purpose" addressPurposeEncoder (_physicalAddressWPPurpose pa)) .
  addressFields (_physicalAddressWPAddress pa)
  where
    addressFields = \case
      AddressSimple a -> fields "simple" simpleAddressEncoder a
      AddressPaf      -> fields "paf" emptyObjEncoder ()
    fields = typeTaggedField "addressUType"

physicalAddressWithPurposeDecoder :: Monad f => Decoder f PhysicalAddressWithPurpose
physicalAddressWithPurposeDecoder = PhysicalAddressWithPurpose
  <$> D.atKey "purpose" addressPurposeDecoder
  <*> addyDecoder

  where
    addyDecoder = typeTaggedDecoder "addressUType" $ \case
      "simple" -> Just $ (TypedTagField AddressSimple simpleAddressDecoder)
      "paf"    -> Just $ (TypedTagField (const AddressPaf) (emptyObjDecoder ()))
      _        -> Nothing


-- | PhysicalAddress <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaphysicaladdress CDR AU v0.1.0 PhysicalAddress >
data PhysicalAddress = PhysicalAddress
  { _physicalAddressAddress :: Address } deriving (Generic, Show, Eq)

physicalAddressEncoder :: Applicative f => Encoder f PhysicalAddress
physicalAddressEncoder = E.mapLikeObj $ \pa ->
  addressFields (_physicalAddressAddress pa)
  where
    addressFields = \case
      AddressSimple a -> fields "simple" simpleAddressEncoder a
      AddressPaf      -> fields "paf" emptyObjEncoder ()
    fields = typeTaggedField "addressUType"

physicalAddressDecoder :: Monad f => Decoder f PhysicalAddress
physicalAddressDecoder = PhysicalAddress
  <$> addyDecoder
  where
    addyDecoder = typeTaggedDecoder "addressUType" $ \case
      "simple" -> Just $ (TypedTagField AddressSimple simpleAddressDecoder)
      "paf"    -> Just $ (TypedTagField (const AddressPaf) (emptyObjDecoder ()))
      _        -> Nothing


data AddressPurpose =
    AddressPurposeRegistered -- ^ "REGISTERED"
  | AddressPurposeMail -- ^ "MAIL"
  | AddressPurposePhysical -- ^ "PHYSICAL"
  | AddressPurposeWork -- ^ "WORK"
  | AddressPurposeOther -- ^ "OTHER"
  deriving (Bounded, Enum, Eq, Ord, Show)

addressPurposeText :: Prism' Text AddressPurpose
addressPurposeText =
  prism (\case
          AddressPurposeRegistered -> "REGISTERED"
          AddressPurposeMail -> "MAIL"
          AddressPurposePhysical -> "PHYSICAL"
          AddressPurposeWork -> "WORK"
          AddressPurposeOther -> "OTHER"
      )
      (\case
          "REGISTERED" -> Right AddressPurposeRegistered
          "MAIL" -> Right AddressPurposeMail
          "PHYSICAL"-> Right AddressPurposePhysical
          "WORK" -> Right AddressPurposeWork
          "OTHER" -> Right AddressPurposeOther
          t -> Left t
      )

addressPurposeEncoder :: Applicative f => Encoder f AddressPurpose
addressPurposeEncoder = E.prismE addressPurposeText E.text

addressPurposeDecoder :: Monad f => Decoder f AddressPurpose
addressPurposeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid address purpose")
  addressPurposeText
  D.text

-- | The type of address object
data Address
  = AddressSimple SimpleAddress
    -- ^ SimpleAddress < https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemasimpleaddress CDR AU v0.1.0 SimpleAddress>
  | AddressPaf -- We don't know what this is yet
    -- ^ <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemapafaddress CDR AU v0.1.0 PAFAddress>
  deriving (Generic, Show, Eq)

data SimpleAddress = SimpleAddress
  { _simpleAddressMailingName  :: Maybe Text -- ^ Name of the individual or business formatted for inclusion in an address used for physical mail.
  , _simpleAddressAddressLine1 :: Text
  , _simpleAddressAddressLine2 :: Maybe Text
  , _simpleAddressAddressLine3 :: Maybe Text
  , _simpleAddressPostcode     :: Maybe Text -- ^ Mandatory for Australian address.
  , _simpleAddressCity         :: Text
  , _simpleAddressState        :: AddressState -- ^ Free text if the country is not Australia. If country is Australia then must be one of the values defined by the <https://www.iso.org/obp/ui/#iso:code:3166:AU ISO 3166:AU> standard.
  , _simpleAddressCountry      :: Maybe Country -- ^ A valid ISO 3166 Alpha-3 country code.
  } deriving (Eq, Show, Generic)


simpleAddressEncoder :: Applicative f => Encoder f SimpleAddress
simpleAddressEncoder = E.mapLikeObj $ \p ->
  maybeOrAbsentE "mailingName" E.text (_simpleAddressMailingName p) .
  E.atKey' "addressLine1" E.text (_simpleAddressAddressLine1 p) .
  maybeOrAbsentE "addressLine2" E.text (_simpleAddressAddressLine2 p) .
  maybeOrAbsentE "addressLine3" E.text (_simpleAddressAddressLine3 p) .
  maybeOrAbsentE "postcode" E.text (_simpleAddressPostcode p) .
  E.atKey' "city" E.text (_simpleAddressCity p) .
  E.atKey' "state" addressStateEncoder (_simpleAddressState p) .
  maybeOrAbsentE "country" countryAlphaThreeEncoder (_simpleAddressCountry p)

simpleAddressDecoder :: Monad f => Decoder f SimpleAddress
simpleAddressDecoder = SimpleAddress
    <$> atKeyOptional' "mailingName" D.text
    <*> D.atKey "addressLine1" D.text
    <*> atKeyOptional' "addressLine2" D.text
    <*> atKeyOptional' "addressLine3" D.text
    <*> atKeyOptional' "postcode" D.text
    <*> D.atKey "city" D.text
    <*> D.atKey "state" addressStateDecoder
    <*> atKeyOptional' "country" countryAlphaThreeDecoder

-- | @AddressState@ If country is Australia then must be one of the values defined by the <https://www.iso.org/obp/ui/#iso:code:3166:AU ISO 3166:AU> standard.
data AddressState =
    AustralianState AustraliaState
  | OtherCountryState Text
  deriving (Show, Eq)

addressStateEncoder :: Applicative f => Encoder f AddressState
addressStateEncoder = E.encodeA $ \case
  AustralianState a -> E.runEncoder australiaStateEncoder a
  OtherCountryState o -> E.runEncoder E.text o

addressStateDecoder :: Monad f => Decoder f AddressState
addressStateDecoder
  =   (AustralianState <$> australiaStateDecoder)
  <!> (OtherCountryState <$> D.text)

-- | State in Austalia, values defined by the <https://www.iso.org/obp/ui/#iso:code:3166:AU ISO 3166:AU> standard.
-- TODO: subdivision name or 3166-2 code?
data AustraliaState =
    AustraliaStateACT -- ^ "Australian Capital Territory"
  | AustraliaStateNSW -- ^ "New South Wales"
  | AustraliaStateNT -- ^ "Northern Territory"
  | AustraliaStateQLD -- ^ "Queensland"
  | AustraliaStateSA -- ^ "South Australia"
  | AustraliaStateTAS -- ^ "Tasmania"
  | AustraliaStateVIC -- ^ "Victoria"
  | AustraliaStateWA -- ^ "Western Australia"
  deriving (Bounded, Enum, Eq, Ord, Show)

australiaStateText ::
  Prism' Text AustraliaState
australiaStateText =
  prism (\case
          AustraliaStateACT -> "AU-ACT"
          AustraliaStateNSW -> "AU-NSW"
          AustraliaStateNT  -> "AU-NT"
          AustraliaStateQLD -> "AU-QLD"
          AustraliaStateSA  -> "AU-SA"
          AustraliaStateTAS -> "AU-TAS"
          AustraliaStateVIC -> "AU-VIC"
          AustraliaStateWA  -> "AU-WA"
      )
      (\case
          "AU-ACT" -> Right AustraliaStateACT
          "AU-NSW" -> Right AustraliaStateNSW
          "AU-NT"  -> Right AustraliaStateNT
          "AU-QLD" -> Right AustraliaStateQLD
          "AU-SA"  -> Right AustraliaStateSA
          "AU-TAS" -> Right AustraliaStateTAS
          "AU-VIC" -> Right AustraliaStateVIC
          "AU-WA"  -> Right AustraliaStateWA
          t -> Left t
      )

australiaStateEncoder :: Applicative f => Encoder f AustraliaState
australiaStateEncoder = E.prismE australiaStateText E.text

australiaStateDecoder :: Monad f => Decoder f AustraliaState
australiaStateDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid australian state")
  australiaStateText
  D.text

-- | PAFAddress < https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemapafaddress CDR AU v0.1.0 PAFAddress >
data PAFAddress = PAFAddress
  { _pafAddressData :: () -- GenericObject -- ^ TODO  how to specify a generic object
  } deriving (Generic, Show, Eq)
