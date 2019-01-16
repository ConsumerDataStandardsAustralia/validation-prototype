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


data PhoneNumber = PhoneNumber
 { _phoneNumberIsPreferred :: Bool
 , _phoneNumberPurpose     :: PhoneNumberPurpose
 , _phoneNumberCountryCode :: Maybe Text
 , _phoneNumberAreaCode    :: Maybe Text
 , _phoneNumberNumber      :: Text
 , _phoneNumberExtension   :: Maybe Text
 , _phoneNumberFullNumber  :: Text
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
