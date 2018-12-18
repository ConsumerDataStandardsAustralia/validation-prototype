{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Vector.V6 where

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

data V6 a =
  V6 a a a a a a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (V6 a) where
  showsPrec _ = showList . toList

v6List :: Prism' [a] (V6 a)
v6List = prism toList v6FromList

v6FromList :: [a] -> Either [a] (V6 a)
v6FromList [a,b,c,d,e,f] = Right (V6 a b c d e f)
v6FromList as = Left as

v6CharDecoder :: Monad f => Decoder f (V6 Char)
v6CharDecoder =
  D.string >>=
    maybe (throwError err) pure . preview v6List
  where
    err = D.ConversionFailure "Wrong number of characters"

v6CharEncoder :: Applicative f => Encoder f (V6 Char)
v6CharEncoder = Text.pack . review v6List >$< E.text

v6ListEncoder :: Applicative f => Encoder f a -> Encoder f (V6 a)
v6ListEncoder = contramap toList . E.list

v6DigitDecoder :: Monad f => Decoder f (V6 DecDigit)
v6DigitDecoder = v6CharDecoder >>=
  either err pure . traverse (matching charDecimal)
  where
    err :: Monad f => Char -> Decoder f a
    err c = throwError (D.ConversionFailure $ "'" <> Text.singleton c <> "' is not a valid digit")

v6DigitEncoder :: Applicative f => Encoder f (V6 DecDigit)
v6DigitEncoder = Text.pack . toList . fmap (review charDecimal) >$< E.text
