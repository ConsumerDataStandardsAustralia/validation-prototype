{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Vector.V5 where

import Control.Lens
import Control.Monad.Except (throwError)
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Foldable (toList)
import qualified Data.Text as Text

import Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D
import Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E

data V5 a =
  V5 a a a a a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (V5 a) where
  showsPrec _ = showList . toList

v5List :: Prism' [a] (V5 a)
v5List = prism toList v5FromList

v5FromList :: [a] -> Either [a] (V5 a)
v5FromList [a,b,c,d,e] = Right (V5 a b c d e)
v5FromList as = Left as

v5CharDecoder :: Monad f => Decoder f (V5 Char)
v5CharDecoder =
  D.string >>=
    maybe (throwError err) pure . preview v5List
  where
    err = D.ConversionFailure "Wrong number of characters"

v5CharEncoder :: Applicative f => Encoder f (V5 Char)
v5CharEncoder = review v5List >$< stringE

stringE :: Applicative f => Encoder f String
stringE = Text.pack >$< E.text

v5ListEncoder :: Applicative f => Encoder f a -> Encoder f (V5 a)
v5ListEncoder = contramap toList . E.list

v5DigitDecoder :: Monad f => Decoder f (V5 DecDigit)
v5DigitDecoder = v5CharDecoder >>=
  either err pure . traverse (matching charDecimal)
  where
    err :: Monad f => Char -> Decoder f a
    err c = throwError (D.ConversionFailure $ "'" <> Text.singleton c <> "' is not a valid digit")

v5DigitEncoder :: Applicative f => Encoder f (V5 DecDigit)
v5DigitEncoder = toList . fmap (review charDecimal) >$< stringE
