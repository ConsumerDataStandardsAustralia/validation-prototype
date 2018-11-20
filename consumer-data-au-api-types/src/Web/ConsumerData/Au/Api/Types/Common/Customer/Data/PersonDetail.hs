{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.PersonDetail
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.PersonDetail
  , module EmailAddress
  , module PhoneNumber
  , module PhysicalAddress
  ) where

import           Data.List.NonEmpty   (NonEmpty)
import           GHC.Generics         (Generic)
import           Waargonaut.Decode    (Decoder)
import qualified Waargonaut.Decode    as D
import           Waargonaut.Encode    (Encoder)
import qualified Waargonaut.Encode    as E

import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.Person
import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.PhoneNumber as PhoneNumber
import Web.ConsumerData.Au.Api.Types.Common.Customer.Data.EmailAddress as EmailAddress
import Web.ConsumerData.Au.Api.Types.Data.PhysicalAddress as PhysicalAddress

-- | The individual who authorised the session.
-- <https://consumerdatastandardsaustralia.github.io/standards/#schemapersondetail>
data PersonDetail = PersonDetail
  { _personDetailPerson            :: Person
  , _personDetailPhoneNumbers      :: NonEmpty PhoneNumber
  , _personDetailEmailAddresses    :: [EmailAddress]
  , _personDetailPhysicalAddresses :: [PhysicalAddress]
  }
  deriving (Generic, Eq, Show)


personDetailEncoder :: Applicative f => Encoder f PersonDetail
personDetailEncoder = E.mapLikeObj $ \p ->
  personFields (_personDetailPerson p) .
  E.atKey' "phoneNumbers" (E.nonempty phoneNumberEncoder) (_personDetailPhoneNumbers p) .
  E.atKey' "emailAddresses" (E.list emailAddressEncoder) (_personDetailEmailAddresses p) .
  E.atKey' "physicalAddresses" (E.list physicalAddressEncoder) (_personDetailPhysicalAddresses p)

personDetailDecoder :: Monad f => Decoder f PersonDetail
personDetailDecoder = PersonDetail
  <$> personDecoder
  <*> (D.atKey "phoneNumbers" $ D.nonempty phoneNumberDecoder)
  <*> (D.atKey "emailAddresses" $ D.list emailAddressDecoder)
  <*> (D.atKey "physicalAddresses" $ D.list physicalAddressDecoder)
