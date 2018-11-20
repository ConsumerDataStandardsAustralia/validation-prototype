{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.CustomerDetailResponse
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.CustomerDetailResponse
  , module OrganisationDetail
  , module PersonDetail
  ) where

import           Waargonaut.Decode  (Decoder)
import           Waargonaut.Encode  (Encoder)
import qualified Waargonaut.Encode  as E
import           Waargonaut.Generic (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.OrganisationDetail as OrganisationDetail
import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.PersonDetail as PersonDetail
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers
import Web.ConsumerData.Au.Api.Types.Tag

data CustomerDetailResponse
  = CustomerDetailPerson PersonDetail
  | CustomerDetailOrganisation OrganisationDetail
  deriving (Eq,Show)

customerDetailResponseEncoder :: Applicative f => Encoder f CustomerDetailResponse
customerDetailResponseEncoder = E.mapLikeObj $ \case
  CustomerDetailPerson p -> fields "person" personDetailEncoder p
  CustomerDetailOrganisation o -> fields "organisation" organisationDetailEncoder o
  where
    fields = typeTaggedField "customer$type"

customerDetailResponseDecoder :: Monad f => Decoder f CustomerDetailResponse
customerDetailResponseDecoder = typeTaggedDecoder "customer$type" $ \case
    "person"       -> Just $ (TypedTagField CustomerDetailPerson personDetailDecoder)
    "organisation" -> Just $ (TypedTagField CustomerDetailOrganisation organisationDetailDecoder)
    _              -> Nothing

instance JsonDecode OB CustomerDetailResponse where
  mkDecoder = tagOb customerDetailResponseDecoder

instance JsonEncode OB CustomerDetailResponse where
  mkEncoder = tagOb customerDetailResponseEncoder
