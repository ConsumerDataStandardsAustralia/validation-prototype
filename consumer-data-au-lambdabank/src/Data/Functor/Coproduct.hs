{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

-- | Taken wholesale from Data types a la carte.
module Data.Functor.Coproduct where

data ((f :: * -> *) :+: (g :: * -> *)) e = Inl (f e) | Inr (g e)
  deriving (Functor)
infixr 6 :+:

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPABLE #-} (f :<: g, Functor h)  => f :<: (h :+: g) where
  inj = Inr . inj
