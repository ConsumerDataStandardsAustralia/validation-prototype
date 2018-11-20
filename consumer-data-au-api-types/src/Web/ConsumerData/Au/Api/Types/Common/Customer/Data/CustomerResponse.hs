{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.CustomerResponse
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.CustomerResponse
  , module Organisation
  , module Person
  ) where

import           Waargonaut.Decode        (Decoder)
import           Waargonaut.Encode        (Encoder)
import qualified Waargonaut.Encode        as E
import           Waargonaut.Generic       (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Organisation as Organisation
import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Person as Person
import Web.ConsumerData.Au.Api.Types.Tag
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers

data CustomerResponse
  = CustomerPerson Person
 | CustomerOrganisation Organisation
  deriving (Eq, Show)

customerResponseEncoder :: Applicative f => Encoder f CustomerResponse
customerResponseEncoder = E.mapLikeObj $ \case
  CustomerPerson p -> fields "person" personEncoder p
  CustomerOrganisation o -> fields "organisation" organisationEncoder o
  where
    fields = typeTaggedField "customer$type"

customerResponseDecoder :: Monad f => Decoder f CustomerResponse
customerResponseDecoder = typeTaggedDecoder "customer$type" $ \case
    "person"       -> Just $ (TypedTagField CustomerPerson personDecoder)
    "organisation" -> Just $ (TypedTagField CustomerOrganisation organisationDecoder)
    _              -> Nothing

instance JsonDecode OB CustomerResponse where
  mkDecoder = tagOb customerResponseDecoder

instance JsonEncode OB CustomerResponse where
  mkEncoder = tagOb customerResponseEncoder
