{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AuPost.PAF.PostalDeliveryType
  ( module AuPost.PAF.PostalDeliveryType
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E


data PostalDeliveryType =
    PdtCarePo -- ^ "CARE PO" = Care Of Post Office  |  Poste Restante
  | PdtCma -- ^ "CMA" = Community Mail Agent
  | PdtCmd -- ^ "CMB" = Community Mail Bag
  | PdtGpoBox -- ^ "GPO BOX" = General Post Office Box
  | PdtLockedBag -- ^ "LOCKED BAG" = Locked Mail Bag Service
  | PdtMs -- ^ "MS" = Mail Service
  | PdtPoBox -- ^ "PO BOX" = Post Office Box
  | PdtPrivateBag -- ^ "PRIVATE BAG" = Private Mail Bag Service
  | PdtRsd -- ^ "RSD" = Roadside Delivery
  | PdtRmb -- ^ "RMB" = Roadside Mail Bag  |  Roadside Mail Box
  | PdtRms -- ^ "RMS" = Roadside Mail Service
  | PdtCpa -- ^ "CPA" = Community Postal Agent
  deriving (Bounded, Enum, Eq, Ord, Show)

postalDeliveryTypeText ::
  Prism' Text PostalDeliveryType
postalDeliveryTypeText =
  prism (\case
            PdtCarePo -> "CARE PO"
            PdtCma -> "CMA"
            PdtCmd -> "CMB"
            PdtGpoBox -> "GPO BOX"
            PdtLockedBag -> "LOCKED BAG"
            PdtMs -> "MS"
            PdtPoBox -> "PO BOX"
            PdtPrivateBag -> "PRIVATE BAG"
            PdtRsd -> "RSD"
            PdtRmb -> "RMB"
            PdtRms -> "RMS"
            PdtCpa -> "CPA"
        )
        (\case
            "CARE PO" -> Right PdtCarePo
            "CMA" -> Right PdtCma
            "CMB" -> Right PdtCmd
            "GPO BOX" -> Right PdtGpoBox
            "LOCKED BAG" -> Right PdtLockedBag
            "MS" -> Right PdtMs
            "PO BOX" -> Right PdtPoBox
            "PRIVATE BAG" -> Right PdtPrivateBag
            "RSD" -> Right PdtRsd
            "RMB" -> Right PdtRmb
            "RMS" -> Right PdtRms
            "CPA" -> Right PdtCpa
            t -> Left t
        )

postalDeliveryTypeEncoder :: Applicative f => Encoder f PostalDeliveryType
postalDeliveryTypeEncoder = E.prismE postalDeliveryTypeText E.text

postalDeliveryTypeDecoder :: Monad f => Decoder f PostalDeliveryType
postalDeliveryTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid PAF PostalDeliveryType")
  postalDeliveryTypeText
  D.text
