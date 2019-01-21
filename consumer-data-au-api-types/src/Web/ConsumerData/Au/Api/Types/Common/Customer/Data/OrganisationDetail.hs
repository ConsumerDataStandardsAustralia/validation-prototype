{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.OrganisationDetail
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.OrganisationDetail
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           GHC.Generics       (Generic)
import           Waargonaut.Decode  (Decoder)
import qualified Waargonaut.Decode  as D
import           Waargonaut.Encode  (Encoder)
import qualified Waargonaut.Encode  as E

import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation
    (Organisation, organisationDecoder, organisationFields)
import Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress
    (PhysicalAddressWithPurpose, physicalAddressWithPurposeDecoder,
    physicalAddressWithPurposeEncoder)


data OrganisationDetail = OrganisationDetail
  { _organisationDetailOrganisation :: Organisation
  , _organisationDetailAddresses    :: NonEmpty PhysicalAddressWithPurpose
  }
  deriving (Generic, Eq, Show)

organisationDetailEncoder :: Applicative f => Encoder f OrganisationDetail
organisationDetailEncoder = E.mapLikeObj $ \od ->
  organisationFields (_organisationDetailOrganisation od) .
  E.atKey' "physicalAddresses" (E.nonempty physicalAddressWithPurposeEncoder) (_organisationDetailAddresses od)

organisationDetailDecoder :: Monad f => Decoder f OrganisationDetail
organisationDetailDecoder = OrganisationDetail
  <$> organisationDecoder
  <*> (D.atKey "physicalAddresses" (D.nonempty physicalAddressWithPurposeDecoder))
