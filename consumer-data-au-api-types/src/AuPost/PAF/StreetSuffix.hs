{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AuPost.PAF.StreetSuffix
  ( module AuPost.PAF.StreetSuffix
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E


data StreetSuffix =
    StreetSuffixCN -- ^ "CN" = Central
  | StreetSuffixNW -- ^ "NW" = North West
  | StreetSuffixE  -- ^ "E"  = East
  | StreetSuffixS  -- ^ "S"  = South
  | StreetSuffixEX -- ^ "EX" = Extension
  | StreetSuffixSE -- ^ "SE" = South East
  | StreetSuffixLR -- ^ "LR" = Lower
  | StreetSuffixSW -- ^ "SW" = South West
  | StreetSuffixN  -- ^ "N"  = North
  | StreetSuffixUP -- ^ "UP" = Upper
  | StreetSuffixNE -- ^ "NE" = North East
  | StreetSuffixW  -- ^ "W"  = West
  deriving (Bounded, Enum, Eq, Ord, Show)

streetSuffixText ::
  Prism' Text StreetSuffix
streetSuffixText =
  prism (\case
            StreetSuffixCN -> "CN"
            StreetSuffixNW -> "NW"
            StreetSuffixE  -> "E"
            StreetSuffixS  -> "S"
            StreetSuffixEX -> "EX"
            StreetSuffixSE -> "SE"
            StreetSuffixLR -> "LR"
            StreetSuffixSW -> "SW"
            StreetSuffixN  -> "N"
            StreetSuffixUP -> "UP"
            StreetSuffixNE -> "NE"
            StreetSuffixW  -> "W"
        )
        (\case
            "CN" -> Right StreetSuffixCN
            "NW" -> Right StreetSuffixNW
            "E"  -> Right StreetSuffixE
            "S"  -> Right StreetSuffixS
            "EX" -> Right StreetSuffixEX
            "SE" -> Right StreetSuffixSE
            "LR" -> Right StreetSuffixLR
            "SW" -> Right StreetSuffixSW
            "N"  -> Right StreetSuffixN
            "UP" -> Right StreetSuffixUP
            "NE" -> Right StreetSuffixNE
            "W"  -> Right StreetSuffixW
            t -> Left t
        )

streetSuffixEncoder :: Applicative f => Encoder f StreetSuffix
streetSuffixEncoder = E.prismE streetSuffixText E.text

streetSuffixDecoder :: Monad f => Decoder f StreetSuffix
streetSuffixDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid PAF StreetSuffix")
  streetSuffixText
  D.text
