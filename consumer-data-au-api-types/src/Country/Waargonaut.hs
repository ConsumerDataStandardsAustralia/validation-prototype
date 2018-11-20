{-# LANGUAGE OverloadedStrings #-}
module Country.Waargonaut where

import Control.Lens (Prism', prism', (#))
import Country  (Country, alphaThreeUpper, decodeAlphaThree)
import Data.Text (Text)
import Waargonaut.Decode.Error as D
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Encode          as E

_CountryAlphaThreeUpper :: Prism' Text Country
_CountryAlphaThreeUpper = prism' alphaThreeUpper decodeAlphaThree

countryAlphaThreeEncoder :: Applicative f => E.Encoder f Country
countryAlphaThreeEncoder = E.prismE _CountryAlphaThreeUpper E.text

countryAlphaThreeDecoder :: Monad m => D.Decoder m Country
countryAlphaThreeDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid three letter country code")
  _CountryAlphaThreeUpper
  D.text
