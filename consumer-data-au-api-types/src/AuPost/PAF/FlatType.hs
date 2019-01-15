{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AuPost.PAF.FlatType
  ( module AuPost.PAF.FlatType
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E


data FlatType =
    FlatTypeAPT -- ^ Apartment APT
  | FlatTypeCTGE -- ^ Cottage CTGE
  | FlatTypeDUP -- ^ Duplex DUP
  | FlatTypeFY -- ^ Factory FY
  | FlatTypeF -- ^ Flat F
  | FlatTypeHSE -- ^ House HSE
  | FlatTypeKSK -- ^ Kiosk KSK
  | FlatTypeMSNT -- ^ Maisonette MSNT
  | FlatTypeMB -- ^ Marine Berth MB
  | FlatTypeOFF -- ^ Office OFF
  | FlatTypePTHS -- ^ Penthouse PTHS
  | FlatTypeR -- ^ Rear R
  | FlatTypeRM -- ^ Room RM
  | FlatTypeSHED -- ^ Shed SHED
  | FlatTypeSHOP -- ^ Shop SHOP
  | FlatTypeSITE -- ^ Site SITE
  | FlatTypeSL -- ^ Stall SL
  | FlatTypeSTU -- ^ Studio STU
  | FlatTypeSE -- ^ Suite SE
  | FlatTypeTNHS -- ^ Townhouse TNHS
  | FlatTypeU -- ^ Unit U
  | FlatTypeVLLA -- ^ Villa VLLA
  | FlatTypeWARD -- ^ Ward WARD
  | FlatTypeWE -- ^ Warehouse WE
  deriving (Bounded, Enum, Eq, Ord, Show)

flatTypeText ::
  Prism' Text FlatType
flatTypeText =
  prism (\case
            FlatTypeAPT -> "APT"
            FlatTypeCTGE -> "CTGE"
            FlatTypeDUP -> "DUP"
            FlatTypeFY -> "FY"
            FlatTypeF -> "F"
            FlatTypeHSE -> "HSE"
            FlatTypeKSK -> "KSK"
            FlatTypeMSNT -> "MSNT"
            FlatTypeMB -> "MB"
            FlatTypeOFF -> "OFF"
            FlatTypePTHS -> "PTHS"
            FlatTypeR -> "R"
            FlatTypeRM -> "RM"
            FlatTypeSHED -> "SHED"
            FlatTypeSHOP -> "SHOP"
            FlatTypeSITE -> "SITE"
            FlatTypeSL -> "SL"
            FlatTypeSTU -> "STU"
            FlatTypeSE -> "SE"
            FlatTypeTNHS -> "TNHS"
            FlatTypeU -> "U"
            FlatTypeVLLA -> "VLLA"
            FlatTypeWARD -> "WARD"
            FlatTypeWE -> "WE"
        )
        (\case
            "APT" -> Right FlatTypeAPT
            "CTGE" -> Right FlatTypeCTGE
            "DUP" -> Right FlatTypeDUP
            "FY" -> Right FlatTypeFY
            "F" -> Right FlatTypeF
            "HSE" -> Right FlatTypeHSE
            "KSK" -> Right FlatTypeKSK
            "MSNT" -> Right FlatTypeMSNT
            "MB" -> Right FlatTypeMB
            "OFF" -> Right FlatTypeOFF
            "PTHS" -> Right FlatTypePTHS
            "R" -> Right FlatTypeR
            "RM" -> Right FlatTypeRM
            "SHED" -> Right FlatTypeSHED
            "SHOP" -> Right FlatTypeSHOP
            "SITE" -> Right FlatTypeSITE
            "SL" -> Right FlatTypeSL
            "STU" -> Right FlatTypeSTU
            "SE" -> Right FlatTypeSE
            "TNHS" -> Right FlatTypeTNHS
            "U" -> Right FlatTypeU
            "VLLA" -> Right FlatTypeVLLA
            "WARD" -> Right FlatTypeWARD
            "WE" -> Right FlatTypeWE
            t -> Left t
        )

flatTypeEncoder :: Applicative f => Encoder f FlatType
flatTypeEncoder = E.prismE flatTypeText E.text

flatTypeDecoder :: Monad f => Decoder f FlatType
flatTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid PAF FlatType")
  flatTypeText
  D.text
