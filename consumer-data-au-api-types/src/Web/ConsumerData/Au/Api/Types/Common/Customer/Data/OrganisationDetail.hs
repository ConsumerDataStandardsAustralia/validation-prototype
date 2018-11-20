{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.OrganisationDetail
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.OrganisationDetail
  ) where

import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation
import Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress

-- | The individual who authorised the session.
-- <https://consumerdatastandardsaustralia.github.io/standards/#schemaorganisationdetail>
data OrganisationDetail = OrganisationDetail
  { _organisationDetailOrganisation :: Organisation
  , _organisationDetailAddresses    :: [PhysicalAddress]
  }
  deriving (Generic, Eq, Show)

organisationDetailEncoder :: Applicative f => Encoder f OrganisationDetail
organisationDetailEncoder = E.mapLikeObj $ \od ->
  organisationFields (_organisationDetailOrganisation od) .
  E.atKey' "physicalAddresses" (E.list physicalAddressEncoder) (_organisationDetailAddresses od)

organisationDetailDecoder :: Monad f => Decoder f OrganisationDetail
organisationDetailDecoder = OrganisationDetail
  <$> organisationDecoder
  <*> (D.atKey "physicalAddresses" (D.list physicalAddressDecoder))
