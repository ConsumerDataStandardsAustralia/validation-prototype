{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.EmailAddress
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.EmailAddress
  ) where

import           Control.Lens               (Prism', prism, ( # ))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

-- | EmailAddress <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaemailaddress CDR AU v0.1.0 EmailAddress >
data EmailAddress = EmailAddress
 { _emailAddressIsPreferred :: Bool    -- ^ Required to be true for one and only one entry to indicate the preferred email address.
 , _emailAddressPurpose     :: EmailAddressPurpose -- ^ The purpose of the address as specified by the customer.
 , _emailAddressAddress     :: Text -- ^ The email address value formatted according to <https://tools.ietf.org/html/rfc5322 RFC 5322>.
                                -- TODO Do we need to restict formatting on email address as per standard?
 } deriving (Generic, Show, Eq)

emailAddressEncoder :: Applicative f => Encoder f EmailAddress
emailAddressEncoder = E.mapLikeObj $ \pn ->
  E.atKey' "isPreferred" E.bool (_emailAddressIsPreferred pn) .
  E.atKey' "purpose" emailAddressPurposeEncoder (_emailAddressPurpose pn) .
  E.atKey' "address" E.text (_emailAddressAddress pn)

emailAddressDecoder :: Monad f => Decoder f EmailAddress
emailAddressDecoder = EmailAddress
  <$> (D.atKey "isPreferred" D.bool)
  <*> (D.atKey "purpose" emailAddressPurposeDecoder)
  <*> (D.atKey "address" D.text)

-- | The purpose of the email address.
data EmailAddressPurpose =
    EmailAddressPurposeWork -- ^ "WORK"
  | EmailAddressPurposeHome -- ^ "HOME"
  | EmailAddressPurposeOther -- ^ "OTHER"
  | EmailAddressPurposeUnspecified -- ^ "UNSPECIFIED"
  deriving (Bounded, Enum, Eq, Ord, Show)

emailAddressPurposeText :: Prism' Text EmailAddressPurpose
emailAddressPurposeText =
  prism (\case
          EmailAddressPurposeWork -> "WORK"
          EmailAddressPurposeHome -> "HOME"
          EmailAddressPurposeOther -> "OTHER"
          EmailAddressPurposeUnspecified -> "UNSPECIFIED"
      )
      (\case
          "WORK" -> Right EmailAddressPurposeWork
          "HOME" -> Right EmailAddressPurposeHome
          "OTHER"-> Right EmailAddressPurposeOther
          "UNSPECIFIED" -> Right EmailAddressPurposeUnspecified
          t -> Left t
      )

emailAddressPurposeEncoder :: Applicative f => Encoder f EmailAddressPurpose
emailAddressPurposeEncoder = E.prismE emailAddressPurposeText E.text

emailAddressPurposeDecoder :: Monad f => Decoder f EmailAddressPurpose
emailAddressPurposeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Is not a valid email address purpose")
  emailAddressPurposeText
  D.text
