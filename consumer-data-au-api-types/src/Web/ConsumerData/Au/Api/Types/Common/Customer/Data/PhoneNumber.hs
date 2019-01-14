{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.PhoneNumber
  ( module Web.ConsumerData.Au.Api.Types.Common.Customer.Data.PhoneNumber
  ) where

import           Control.Lens                        (Prism', prism, (#))
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Waargonaut.Decode                   (Decoder)
import qualified Waargonaut.Decode                   as D
import qualified Waargonaut.Decode.Error             as D
import           Waargonaut.Encode                   (Encoder)
import qualified Waargonaut.Encode                   as E

import           Waargonaut.Helpers                  (atKeyOptional', maybeOrAbsentE)

-- | PhoneNumber <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaphonenumber CDR AU v0.1.0 PhoneNumber >
data PhoneNumber = PhoneNumber
 { _phoneNumberIsPreferred :: Bool               -- ^ Required to be true for one and only one entry to indicate the preferred phone number
 , _phoneNumberPurpose     :: PhoneNumberPurpose -- ^ The purpose of the number as specified by the customer.
 , _phoneNumberCountryCode :: Maybe Text         -- ^ If absent, should be assumed to be +61 for Australia. The + should be included.
 , _phoneNumberAreaCode    :: Maybe Text         -- ^ Required for non-mobile phone numbers. If this field is present and refers to an Australian area code, then the leading '0' should not be included.
 , _phoneNumberNumber      :: Text               -- ^ The actual phone number with leading zeroes as appropriate.
 , _phoneNumberExtension   :: Maybe Text         -- ^    An extension number (if applicable).
 , _phoneNumberFullNumber  :: Text               -- ^ Fully formatted phone number with country code, area code, number and extension incorporated. Formatted according to <https://tools.ietf.org/html/rfc3966#section-5.1.4 RFC 3966 section 5.1.4.>.
 } deriving (Generic, Show, Eq)

phoneNumberEncoder :: Applicative f => Encoder f PhoneNumber
phoneNumberEncoder = E.mapLikeObj $ \pn ->
  E.atKey' "isPreferred" E.bool (_phoneNumberIsPreferred pn) .
  E.atKey' "purpose" phoneNumberPurposeEncoder (_phoneNumberPurpose pn) .
  maybeOrAbsentE "countryCode" E.text (_phoneNumberCountryCode pn) .
  maybeOrAbsentE "areaCode" E.text (_phoneNumberAreaCode pn) .
  E.atKey' "number" E.text (_phoneNumberNumber pn) .
  maybeOrAbsentE "extension" E.text (_phoneNumberExtension pn) .
  E.atKey' "fullNumber" E.text (_phoneNumberFullNumber pn)

phoneNumberDecoder :: Monad f => Decoder f PhoneNumber
phoneNumberDecoder = PhoneNumber
  <$> D.atKey "isPreferred" D.bool
  <*> D.atKey "purpose" phoneNumberPurposeDecoder
  <*> atKeyOptional' "countryCode" D.text
  <*> atKeyOptional' "areaCode"  D.text
  <*> D.atKey "number" D.text
  <*> atKeyOptional' "extension" D.text
  <*> D.atKey "fullNumber" D.text


-- | The purpose of the phone number.
data PhoneNumberPurpose =
    PhoneNumberPurposeMobile -- ^ "MOBILE"
  | PhoneNumberPurposeWork -- ^ "WORK"
  | PhoneNumberPurposeHome -- ^ "HOME"
  | PhoneNumberPurposeOther -- ^ "OTHER"
  | PhoneNumberPurposeInternational -- ^ "INTERNATIONAL"
  | PhoneNumberPurposeUnspecified -- ^ "UNSPECIFIED"
  deriving (Bounded, Enum, Eq, Ord, Show)

phoneNumberPurposeText :: Prism' Text PhoneNumberPurpose
phoneNumberPurposeText =
  prism (\case
          PhoneNumberPurposeMobile -> "MOBILE"
          PhoneNumberPurposeWork -> "WORK"
          PhoneNumberPurposeHome -> "HOME"
          PhoneNumberPurposeOther -> "OTHER"
          PhoneNumberPurposeInternational -> "INTERNATIONAL"
          PhoneNumberPurposeUnspecified -> "UNSPECIFIED"
      )
      (\case
          "MOBILE" -> Right PhoneNumberPurposeMobile
          "WORK" -> Right PhoneNumberPurposeWork
          "HOME" -> Right PhoneNumberPurposeHome
          "OTHER"-> Right PhoneNumberPurposeOther
          "INTERNATIONAL" -> Right PhoneNumberPurposeInternational
          "UNSPECIFIED" -> Right PhoneNumberPurposeUnspecified
          t -> Left t
      )

phoneNumberPurposeEncoder :: Applicative f => Encoder f PhoneNumberPurpose
phoneNumberPurposeEncoder = E.prismE phoneNumberPurposeText E.text

phoneNumberPurposeDecoder :: Monad f => Decoder f PhoneNumberPurpose
phoneNumberPurposeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid phone number purpose")
  phoneNumberPurposeText
  D.text
