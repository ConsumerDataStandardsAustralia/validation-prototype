{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Country                 (Country)
import           Country.Waargonaut
    (countryAlphaThreeDecoder, countryAlphaThreeEncoder)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Data.Time.Waargonaut    (utcTimeDecoder, utcTimeEncoder)
import           GHC.Generics            (Generic)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import           Waargonaut.Decode.Error (_ConversionFailure)
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json   (Json)

-- | The authorisation was given to a business agent and this type represents that business. This type should not be used where a retail customer was authorised.
-- <https://consumerdatastandardsaustralia.github.io/standards/?swagger#tocCommonCommonSchemas CDR AU v0.1.0>
data Organisation = Organisation
  -- Last Update time in the new swagger is mandatory everywhere
  { _organisationLastUpdateTime    :: UTCTime -- ^ The date and time this this record was last updated.
  , _organisationAgentFirstName    :: Maybe Text -- ^ WARNING: firstName is optional as per specification!!! The first name of the individual providing access on behalf of the organisation. For people with single names this field need not be present. The single name should be in the lastName field
  , _organisationAgentLastName     :: Text -- ^ The last name of the individual providing access on behalf of the organisation. For people with single names the single name should be in this field
  , _organisationAgentRole         :: Text -- ^     The role of the individual identifed by the Person record in this organisation. Expected to be used for display. Default to “Unspecified” if the role is not known
  , _organisationBusinessName      :: Text -- ^ Name of the organisation.
  , _organisationLegalName         :: Maybe Text -- ^ Legal name, if different to the business name.
  , _organisationShortName         :: Maybe Text -- ^ Short name used for communication, if different to the business name.
  , _organisationAbn               :: Maybe Text -- ^ Australian Business Number.
  , _organisationAcn               :: Maybe Text -- ^ Australian Company Number.
  , _organisationIsACNCRegistered  :: Maybe Bool -- ^ @True@ if registered with the ACNC. @False@ if not. @Absent@ or @null@ if not confirmed.
  , _organisationIndustryCode      :: Maybe Text -- ^ ANZIC (2006) code for the organisation.
  , _organisationOrganisationType  :: Maybe OrganisationType
  , _organisationRegisteredCountry :: Maybe Country -- ^ A valid ISO 3166 Alpha-3 country code.
  , _organisationEstablishmentDate :: Maybe UTCTime -- ^ The date the organisation described was established.
  }
  deriving (Generic, Eq, Show)

organisationEncoder :: Applicative f => Encoder f Organisation
organisationEncoder = E.mapLikeObj organisationFields

organisationFields
  :: (Monoid ws, Semigroup ws)
  => Organisation -> MapLikeObj ws Json -> MapLikeObj ws Json
organisationFields o =
  E.atKey' "lastUpdateTime" utcTimeEncoder (_organisationLastUpdateTime o ) .
  E.atKey' "agentFirstName" (E.maybeOrNull E.text) (_organisationAgentFirstName o ) .
  E.atKey' "agentLastName" E.text (_organisationAgentLastName o ) .
  E.atKey' "agentRole" E.text (_organisationAgentRole o ) .
  E.atKey' "businessName" E.text (_organisationBusinessName o ) .
  E.atKey' "legalName" (E.maybeOrNull E.text) (_organisationLegalName o ) .
  E.atKey' "shortName" (E.maybeOrNull E.text) (_organisationShortName o ) .
  E.atKey' "abn" (E.maybeOrNull E.text) (_organisationAbn o ) .
  E.atKey' "acn" (E.maybeOrNull E.text) (_organisationAcn o ) .
  E.atKey' "isACNRegistered" (E.maybeOrNull E.bool) (_organisationIsACNCRegistered o ) .
  E.atKey' "industryCode" (E.maybeOrNull E.text) (_organisationIndustryCode o ) .
  E.atKey' "organisationType" (E.maybeOrNull organisationTypeEncoder) (_organisationOrganisationType o ) .
  E.atKey' "registeredCountry" (E.maybeOrNull countryAlphaThreeEncoder) (_organisationRegisteredCountry o ) .
  E.atKey' "establishmentDate" (E.maybeOrNull utcTimeEncoder) (_organisationEstablishmentDate o )

organisationDecoder :: Monad f => Decoder f Organisation
organisationDecoder = D.withCursor $ \c -> do
  o <- D.down c
  Organisation
    <$> (D.fromKey "lastUpdateTime" utcTimeDecoder o)
    <*> (D.fromKey "agentFirstName" (D.maybeOrNull D.text) o)
    <*> (D.fromKey "agentLastName" D.text o)
    <*> (D.fromKey "agentRole" D.text o)
    <*> (D.fromKey "businessName" D.text o)
    <*> (D.fromKey "legalName" (D.maybeOrNull D.text) o)
    <*> (D.fromKey "shortName" (D.maybeOrNull D.text) o)
    <*> (D.fromKey "abn" (D.maybeOrNull D.text) o)
    <*> (D.fromKey "acn" (D.maybeOrNull D.text) o)
    <*> (D.fromKey "isACNRegistered" (D.maybeOrNull D.bool) o)
    <*> (D.fromKey "industryCode" (D.maybeOrNull D.text) o)
    <*> (D.fromKey "organisationType" (D.maybeOrNull organisationTypeDecoder) o)
    <*> (D.fromKey "registeredCountry" (D.maybeOrNull countryAlphaThreeDecoder) o)
    <*> (D.fromKey "establishmentDate" (D.maybeOrNull utcTimeDecoder) o)

-- | List of organisation types.
data OrganisationType =
    OrgTypeSoleTrader -- ^ "SOLE_TRADER"
  | OrgTypeCompany -- ^ "COMPANY"
  | OrgTypePartnership -- ^ "PARTNERSHIP"
  | OrgTypeTrust -- ^ "TRUST"
  | OrgTypeGovermentEntity -- ^ "GOVERNMENT_ENTITY"
  | OrgTypeOther -- ^ "OTHER"
  deriving (Show, Eq)

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
