{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module AuPost.PAF.StateType
  ( module AuPost.PAF.StateType
  ) where

import           Control.Lens            (Prism', prism, ( # ))
import           Data.Text               (Text)
import           Waargonaut.Decode       (Decoder)
import qualified Waargonaut.Decode       as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode       (Encoder)
import qualified Waargonaut.Encode       as E


data StateType =
    StateAAT -- ^ "AAT" = Australian Antarctic Territory
  | StateACT -- ^ "ACT" = Australian Capital Territory
  | StateNSW -- ^ "NSW" = New South Wales
  | StateNT  -- ^ "NT"  = Northern Territory
  | StateQLD -- ^ "QLD" = Queensland
  | StateSA  -- ^ "SA"  = South Australia
  | StateTAS -- ^ "TAS" = Tasmania
  | StateVIC -- ^ "VIC" = Victoria
  | StateWA  -- ^ "WA"  = Western Australia
  deriving (Bounded, Enum, Eq, Ord, Show)

stateTypeText ::
  Prism' Text StateType
stateTypeText =
  prism (\case
            StateAAT -> "AAT"
            StateACT -> "ACT"
            StateNSW -> "NSW"
            StateNT  -> "NT"
            StateQLD -> "QLD"
            StateSA  -> "SA"
            StateTAS -> "TAS"
            StateVIC -> "VIC"
            StateWA  -> "WA"
        )
        (\case
            "AAT" -> Right StateAAT
            "ACT" -> Right StateACT
            "NSW" -> Right StateNSW
            "NT"  -> Right StateNT
            "QLD" -> Right StateQLD
            "SA"  -> Right StateSA
            "TAS" -> Right StateTAS
            "VIC" -> Right StateVIC
            "WA"  -> Right StateWA
            t -> Left t
        )

stateTypeEncoder :: Applicative f => Encoder f StateType
stateTypeEncoder = E.prismE stateTypeText E.text

stateTypeDecoder :: Monad f => Decoder f StateType
stateTypeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid PAF StateType")
  stateTypeText
  D.text
