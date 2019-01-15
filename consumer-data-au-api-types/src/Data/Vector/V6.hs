{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Vector.V6 where

import Control.Applicative(liftA2)
import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.Zip(MonadZip(mzipWith))
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal
import Data.Functor.Apply(Apply)
import Data.Functor.Bind(Bind((>>-)))
import qualified Data.Functor.Apply as Apply((<.>))
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Foldable (toList)
import Data.Functor.Classes
import Data.Maybe(fromMaybe)
import Data.Semigroup.Foldable
import qualified Data.Text as Text

import Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D
import Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E

data V6 a =
  V6 a a a a a a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Eq1 V6 where
  liftEq f (V6 a1 a2 a3 a4 a5 a6) (V6 b1 b2 b3 b4 b5 b6) =
    f a1 b1 && f a2 b2 && f a3 b3 && f a4 b4 && f a5 b5 && f a6 b6

instance Ord1 V6 where
  liftCompare f (V6 a1 a2 a3 a4 a5 a6) (V6 b1 b2 b3 b4 b5 b6) =
    f a1 b1 `mappend` f a2 b2 `mappend` f a3 b3 `mappend` f a4 b4 `mappend` f a5 b5 `mappend` f a6 b6

instance Show1 V6 where
  liftShowsPrec f k d (V6 a1 a2 a3 a4 a5 a6) =
    liftShowsPrec f k d [a1, a2, a3, a4, a5, a6]

instance Apply V6 where
  V6 f1 f2 f3 f4 f5 f6 <.> V6 a1 a2 a3 a4 a5 a6 =
    V6 (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5) (f6 a6)

instance Applicative V6 where
  (<*>) =
    (Apply.<.>)
  pure a =
    V6 a a a a a a

instance Bind V6 where
  V6 a1 a2 a3 a4 a5 a6 >>- f =
    let V6 a1' _ _ _ _ _ =
          f a1
        V6 _ a2' _ _ _ _ =
          f a2
        V6 _ _ a3' _ _ _ =
          f a3
        V6 _ _ _ a4' _ _ =
          f a4
        V6 _ _ _ _ a5' _ =
          f a5
        V6 _ _ _ _ _ a6' =
          f a6
    in  V6 a1' a2' a3' a4' a5' a6'

instance Monad V6 where
  (>>=) =
    (>>-)
  return =
    pure

instance Show a => Show (V6 a) where
  showsPrec _ = showList . toList

type instance Index (V6 a) = Int
type instance IxValue (V6 a) = a

instance At (V6 a) where
  at n f v6@(V6 a1 a2 a3 a4 a5 a6) =
    let f' = f . Just
    in  case n of
          0 ->
            fmap (\a1' -> V6 (fromMaybe a1 a1') a2 a3 a4 a5 a6) (f' a1)
          1 ->
            fmap (\a2' -> V6 a1 (fromMaybe a2 a2') a3 a4 a5 a6) (f' a2)
          2 ->
            fmap (\a3' -> V6 a1 a2 (fromMaybe a3 a3') a4 a5 a6) (f' a3)
          3 ->
            fmap (\a4' -> V6 a1 a2 a3 (fromMaybe a4 a4') a5 a6) (f' a4)
          4 ->
            fmap (\a5' -> V6 a1 a2 a3 a4 (fromMaybe a5 a5') a6) (f' a5)
          5 ->
            fmap (\a6' -> V6 a1 a2 a3 a4 a5 (fromMaybe a6 a6')) (f' a6)
          _ ->
            v6 <$ f Nothing

instance Ixed (V6 a) where
  ix n =
    at n . traverse

instance Each (V6 a) (V6 b) a b where
  each =
    traverse

instance Reversing (V6 a) where
  reversing (V6 a1 a2 a3 a4 a5 a6) =
    V6 a6 a5 a4 a3 a2 a1

instance MonadZip V6 where
  mzipWith =
    liftA2

instance Field1 (V6 a) (V6 a) a a where
  _1 f (V6 a1 a2 a3 a4 a5 a6) =
    (\a1' -> V6 a1' a2 a3 a4 a5 a6) <$> f a1

instance Field2 (V6 a) (V6 a) a a where
  _2 f (V6 a1 a2 a3 a4 a5 a6) =
    (\a2' -> V6 a1 a2' a3 a4 a5 a6) <$> f a2

instance Field3 (V6 a) (V6 a) a a where
  _3 f (V6 a1 a2 a3 a4 a5 a6) =
    (\a3' -> V6 a1 a2 a3' a4 a5 a6) <$> f a3

instance Field4 (V6 a) (V6 a) a a where
  _4 f (V6 a1 a2 a3 a4 a5 a6) =
    (\a4' -> V6 a1 a2 a3 a4' a5 a6) <$> f a4

instance Field5 (V6 a) (V6 a) a a where
  _5 f (V6 a1 a2 a3 a4 a5 a6) =
    (\a5' -> V6 a1 a2 a3 a4 a5' a6) <$> f a5

instance Field6 (V6 a) (V6 a) a a where
  _6 f (V6 a1 a2 a3 a4 a5 a6) =
    (\a6' -> V6 a1 a2 a3 a4 a5 a6') <$> f a6

instance Foldable1 V6 where
  foldMap1 f (V6 a1 a2 a3 a4 a5 a6) =
    f a1 <> f a2 <> f a3 <> f a4 <> f a5 <> f a6

instance Traversable1 V6 where
  traverse1 f (V6 a1 a2 a3 a4 a5 a6) =
    V6 <$> f a1 Apply.<.> f a2 Apply.<.> f a3 Apply.<.> f a4 Apply.<.> f a5 Apply.<.> f a6

v6List :: Prism' [a] (V6 a)
v6List = prism' toList v6FromList

v6FromList :: [a] -> Maybe (V6 a)
v6FromList [a,b,c,d,e,f] = Just (V6 a b c d e f)
v6FromList _ = Nothing

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
