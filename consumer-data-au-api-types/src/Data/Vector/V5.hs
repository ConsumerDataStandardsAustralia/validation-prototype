{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Vector.V5 where

import Control.Applicative(liftA2)
import Control.Lens
import Control.Monad.Except (throwError)
import Control.Monad.Zip(MonadZip(mzipWith))
import Data.Functor.Classes
import Data.Digit.Char (charDecimal)
import Data.Digit.Decimal
import Data.Functor.Apply(Apply)
import Data.Functor.Bind(Bind((>>-)))
import qualified Data.Functor.Apply as Apply((<.>))
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Foldable (toList)
import Data.Maybe(fromMaybe)
import Data.Semigroup.Foldable(Foldable1(foldMap1))
import qualified Data.Text as Text

import Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D
import Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E

data V5 a =
  V5 a a a a a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Eq1 V5 where
  liftEq f (V5 a1 a2 a3 a4 a5) (V5 b1 b2 b3 b4 b5) =
    f a1 b1 && f a2 b2 && f a3 b3 && f a4 b4 && f a5 b5

instance Ord1 V5 where
  liftCompare f (V5 a1 a2 a3 a4 a5) (V5 b1 b2 b3 b4 b5) =
    f a1 b1 `mappend` f a2 b2 `mappend` f a3 b3 `mappend` f a4 b4 `mappend` f a5 b5

instance Show1 V5 where
  liftShowsPrec f k d (V5 a1 a2 a3 a4 a5) =
    liftShowsPrec f k d [a1, a2, a3, a4, a5]

instance Apply V5 where
  V5 f1 f2 f3 f4 f5 <.> V5 a1 a2 a3 a4 a5 =
    V5 (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)

instance Applicative V5 where
  (<*>) =
    (Apply.<.>)
  pure a =
    V5 a a a a a

instance Bind V5 where
  V5 a1 a2 a3 a4 a5 >>- f =
    let V5 a1' _ _ _ _ =
          f a1
        V5 _ a2' _ _ _ =
          f a2
        V5 _ _ a3' _ _ =
          f a3
        V5 _ _ _ a4' _ =
          f a4
        V5 _ _ _ _ a5' =
          f a5
    in  V5 a1' a2' a3' a4' a5'

instance Monad V5 where
  (>>=) =
    (>>-)
  return =
    pure

instance Show a => Show (V5 a) where
  showsPrec _ = showList . toList

type instance Index (V5 a) = Int
type instance IxValue (V5 a) = a

instance At (V5 a) where
  at n f v5@(V5 a1 a2 a3 a4 a5) =
    let f' = f . Just
    in  case n of
          0 ->
            fmap (\a1' -> V5 (fromMaybe a1 a1') a2 a3 a4 a5) (f' a1)
          1 ->
            fmap (\a2' -> V5 a1 (fromMaybe a2 a2') a3 a4 a5) (f' a2)
          2 ->
            fmap (\a3' -> V5 a1 a2 (fromMaybe a3 a3') a4 a5) (f' a3)
          3 ->
            fmap (\a4' -> V5 a1 a2 a3 (fromMaybe a4 a4') a5) (f' a4)
          4 ->
            fmap (\a5' -> V5 a1 a2 a3 a4 (fromMaybe a5 a5')) (f' a5)
          _ ->
            v5 <$ f Nothing

instance Ixed (V5 a) where
  ix n =
    at n . traverse

instance Each (V5 a) (V5 b) a b where
  each =
    traverse

instance Reversing (V5 a) where
  reversing (V5 a1 a2 a3 a4 a5) =
    V5 a5 a4 a3 a2 a1


instance MonadZip V5 where
  mzipWith =
    liftA2

instance Field1 (V5 a) (V5 a) a a where
  _1 f (V5 a1 a2 a3 a4 a5) =
    (\a1' -> V5 a1' a2 a3 a4 a5) <$> f a1

instance Field2 (V5 a) (V5 a) a a where
  _2 f (V5 a1 a2 a3 a4 a5) =
    (\a2' -> V5 a1 a2' a3 a4 a5) <$> f a2

instance Field3 (V5 a) (V5 a) a a where
  _3 f (V5 a1 a2 a3 a4 a5) =
    (\a3' -> V5 a1 a2 a3' a4 a5) <$> f a3

instance Field4 (V5 a) (V5 a) a a where
  _4 f (V5 a1 a2 a3 a4 a5) =
    (\a4' -> V5 a1 a2 a3 a4' a5) <$> f a4

instance Field5 (V5 a) (V5 a) a a where
  _5 f (V5 a1 a2 a3 a4 a5) =
    (\a5' -> V5 a1 a2 a3 a4 a5') <$> f a5

instance Foldable1 V5 where
  foldMap1 f (V5 a1 a2 a3 a4 a5) =
    f a1 <> f a2 <> f a3 <> f a4 <> f a5

instance Traversable1 V5 where
  traverse1 f (V5 a1 a2 a3 a4 a5) =
    V5 <$> f a1 Apply.<.> f a2 Apply.<.> f a3 Apply.<.> f a4 Apply.<.> f a5

v5List :: Prism' [a] (V5 a)
v5List = prism' toList v5FromList

v5FromList :: [a] -> Maybe (V5 a)
v5FromList [a,b,c,d,e] = Just (V5 a b c d e)
v5FromList _ = Nothing

v5CharDecoder :: Monad f => Decoder f (V5 Char)
v5CharDecoder =
  D.string >>=
    maybe (throwError err) pure . preview v5List
  where
    err = D.ConversionFailure "Wrong number of characters"

v5CharEncoder :: Applicative f => Encoder f (V5 Char)
v5CharEncoder = Text.pack . review v5List >$< E.text

v5ListEncoder :: Applicative f => Encoder f a -> Encoder f (V5 a)
v5ListEncoder = contramap toList . E.list

v5DigitDecoder :: Monad f => Decoder f (V5 DecDigit)
v5DigitDecoder = v5CharDecoder >>=
  either err pure . traverse (matching charDecimal)
  where
    err :: Monad f => Char -> Decoder f a
    err c = throwError (D.ConversionFailure $ "'" <> Text.singleton c <> "' is not a valid digit")

v5DigitEncoder :: Applicative f => Encoder f (V5 DecDigit)
v5DigitEncoder = Text.pack . toList . fmap (review charDecimal) >$< E.text
