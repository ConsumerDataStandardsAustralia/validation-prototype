{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AuPost.PAF.FloorType
  ( module AuPost.PAF.FloorType
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E


data FloorType =
    FloorTypeB -- ^ Basement B
  | FloorTypeFL -- ^ Floor FL
  | FloorTypeG -- ^ Ground Floor G
  | FloorTypeL -- ^ Level L
  | FloorTypeLG -- ^ Lower Ground Floor LG
  | FloorTypeM -- ^ Mezzanine M
  | FloorTypeUG -- ^ Upper Ground Floor UG
  deriving (Bounded, Enum, Eq, Ord, Show)

floorTypeText ::
  Prism' Text FloorType
floorTypeText =
  prism (\case
            FloorTypeB -> "B"
            FloorTypeFL -> "FL"
            FloorTypeG -> "G"
            FloorTypeL -> "L"
            FloorTypeLG -> "LG"
            FloorTypeM -> "M"
            FloorTypeUG -> "UG"
        )
        (\case
            "B" -> Right FloorTypeB
            "FL" -> Right FloorTypeFL
            "G" -> Right FloorTypeG
            "L" -> Right FloorTypeL
            "LG" -> Right FloorTypeLG
            "M" -> Right FloorTypeM
            "UG" -> Right FloorTypeUG
            t -> Left t
        )

floorTypeEncoder :: Applicative f => Encoder f FloorType
floorTypeEncoder = E.prismE floorTypeText E.text

floorTypeDecoder :: Monad f => Decoder f FloorType
floorTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid PAF FloorType")
  floorTypeText
  D.text
