{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation
  ) where

import           Control.Lens               (Prism', prism, ( # ))
import           Country                    (Country)
import           Country.Waargonaut
    (countryAlphaThreeDecoder, countryAlphaThreeEncoder)
import           Data.Digit.Decimal         (DecDigit)
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Time.Waargonaut       (utcTimeDecoder, utcTimeEncoder)
import           Data.Vector.V5             (V5, v5DigitDecoder, v5DigitEncoder)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (_ConversionFailure)
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)


data Organisation = Organisation
  { _organisationLastUpdateTime    :: UTCTime
  , _organisationAgentFirstName    :: Maybe Text
  , _organisationAgentLastName     :: Text
  , _organisationAgentRole         :: Text
  , _organisationBusinessName      :: Text
  , _organisationLegalName         :: Maybe Text
  , _organisationShortName         :: Maybe Text
  , _organisationAbn               :: Maybe Text
  , _organisationAcn               :: Maybe Text
  , _organisationIsACNCRegistered  :: Maybe Bool
  , _organisationIndustryCode      :: Maybe IndustryCode
  , _organisationOrganisationType  :: OrganisationType
  , _organisationRegisteredCountry :: Maybe Country
  , _organisationEstablishmentDate :: Maybe UTCTime
  }
  deriving (Generic, Eq, Show)

organisationEncoder :: Applicative f => Encoder f Organisation
organisationEncoder = E.mapLikeObj organisationFields

organisationFields
  :: (Monoid ws, Semigroup ws)
  => Organisation -> MapLikeObj ws Json -> MapLikeObj ws Json
organisationFields o =
  E.atKey' "lastUpdateTime" utcTimeEncoder (_organisationLastUpdateTime o ) .
  maybeOrAbsentE "agentFirstName" E.text (_organisationAgentFirstName o ) .
  E.atKey' "agentLastName" E.text (_organisationAgentLastName o ) .
  E.atKey' "agentRole" E.text (_organisationAgentRole o ) .
  E.atKey' "businessName" E.text (_organisationBusinessName o ) .
  maybeOrAbsentE "legalName" E.text (_organisationLegalName o ) .
  maybeOrAbsentE "shortName" E.text (_organisationShortName o ) .
  maybeOrAbsentE "abn" E.text (_organisationAbn o ) .
  maybeOrAbsentE "acn" E.text (_organisationAcn o ) .
  maybeOrAbsentE "isACNRegistered" E.bool (_organisationIsACNCRegistered o ) .
  maybeOrAbsentE "industryCode" industryCodeEncoder (_organisationIndustryCode o ) .
  E.atKey' "organisationType" organisationTypeEncoder (_organisationOrganisationType o ) .
  maybeOrAbsentE "registeredCountry" countryAlphaThreeEncoder (_organisationRegisteredCountry o ) .
  maybeOrAbsentE "establishmentDate" utcTimeEncoder (_organisationEstablishmentDate o )

organisationDecoder :: Monad f => Decoder f Organisation
organisationDecoder =
  Organisation
    <$> D.atKey "lastUpdateTime" utcTimeDecoder
    <*> atKeyOptional' "agentFirstName" D.text
    <*> D.atKey "agentLastName" D.text
    <*> D.atKey "agentRole" D.text
    <*> D.atKey "businessName" D.text
    <*> atKeyOptional' "legalName" D.text
    <*> atKeyOptional' "shortName" D.text
    <*> atKeyOptional' "abn" D.text
    <*> atKeyOptional' "acn" D.text
    <*> atKeyOptional' "isACNRegistered" D.bool
    <*> atKeyOptional' "industryCode" industryCodeDecoder
    <*> D.atKey "organisationType" organisationTypeDecoder
    <*> atKeyOptional' "registeredCountry" countryAlphaThreeDecoder
    <*> atKeyOptional' "establishmentDate" utcTimeDecoder

data IndustryCode = IndustryCode { getIndustryCode :: V5 DecDigit }
  deriving (Generic, Eq, Show)

industryCodeEncoder :: Applicative m => Encoder m IndustryCode
industryCodeEncoder = getIndustryCode >$< v5DigitEncoder

industryCodeDecoder :: Monad m => Decoder m IndustryCode
industryCodeDecoder = IndustryCode <$> v5DigitDecoder


data OrganisationType =
    OrgTypeSoleTrader -- ^ "SOLE_TRADER"
  | OrgTypeCompany -- ^ "COMPANY"
  | OrgTypePartnership -- ^ "PARTNERSHIP"
  | OrgTypeTrust -- ^ "TRUST"
  | OrgTypeGovermentEntity -- ^ "GOVERNMENT_ENTITY"
  | OrgTypeOther -- ^ "OTHER"
  deriving (Bounded, Enum, Eq, Ord, Show)

organisationTypeText ::
  Prism' Text OrganisationType
organisationTypeText =
  prism (\case
            OrgTypeSoleTrader -> "SOLE_TRADER"
            OrgTypeCompany -> "COMPANY"
            OrgTypePartnership -> "PARTNERSHIP"
            OrgTypeTrust -> "TRUST"
            OrgTypeGovermentEntity -> "GOVERNMENT_ENTITY"
            OrgTypeOther-> "OTHER"
        )
        (\case
            "SOLE_TRADER" -> Right OrgTypeSoleTrader
            "COMPANY" -> Right OrgTypeCompany
            "PARTNERSHIP" -> Right OrgTypePartnership
            "TRUST" -> Right OrgTypeTrust
            "GOVERNMENT_ENTITY" -> Right OrgTypeGovermentEntity
            "OTHER" -> Right OrgTypeOther
            t -> Left t
        )

organisationTypeEncoder :: Applicative f => Encoder f OrganisationType
organisationTypeEncoder = E.prismE organisationTypeText E.text

organisationTypeDecoder :: Monad f => Decoder f OrganisationType
organisationTypeDecoder = D.prismDOrFail (_ConversionFailure # "") organisationTypeText D.text
