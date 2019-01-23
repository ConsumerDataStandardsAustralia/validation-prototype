{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Auth.Common.Claims where

import           Control.Applicative  ((<|>))
import           Control.Lens         (Lens', lens, (^.))
import           Data.Aeson.Types
    (FromJSON1 (..), Object, ToJSON (..), ToJSON1 (..), Value (Bool, Object),
    toJSON1, withObject, (.:), (.=))
import           Data.Bool            (bool)
import           Data.Functor.Classes (Eq1 (liftEq), Show1 (..))
import qualified Data.HashMap.Strict  as HM
import           GHC.Generics         (Generic1)

data ClaimValue a =
  ClaimValue a
  | ClaimValues [a]
  deriving (Eq, Show, Functor, Generic1)

claimValue ::
  (a -> b)
  -> ([a] -> b)
  -> ClaimValue a
  -> b
claimValue f g = \case
  ClaimValue a -> f a
  ClaimValues as -> g as

claimValueToObject ::
  (a -> Value)
  -> ([a] -> Value)
  -> ClaimValue a
  -> Object
claimValueToObject f g =
  HM.fromList . pure . claimValue (("value" .=) . f) (("values" .=) . g)

instance Eq1 ClaimValue where
  liftEq f cv1 cv2 =
    case (cv1, cv2) of
      (ClaimValue a, ClaimValue b)     -> f a b
      (ClaimValues as, ClaimValues bs) -> and $ zipWith f as bs
      (ClaimValue _, ClaimValues _)    -> False
      (ClaimValues _, ClaimValue _)    -> False

instance Show1 ClaimValue where
  liftShowsPrec f g n cv s =
    claimValue (("ClaimValue " <>) . ($ s) . f n)
               (("ClaimValues " <>) . ($ s) . g)
               cv

instance ToJSON1 ClaimValue where
  liftToJSON f g =
    Object . claimValueToObject f g

instance FromJSON1 ClaimValue where
  liftParseJSON f g =
    withObject "ClaimValue" $ \o ->
      let
        cv = fmap ClaimValue . (f =<<) $ o .: "value"
        cvs = fmap ClaimValues . (g =<<) $ o .: "values"
      in
        cv <|> cvs

data Claim a =
  EssentialClaim (ClaimValue a)
  | NonEssentialClaim (ClaimValue a)
  deriving (Eq, Show, Generic1, Functor)

claim ::
  (ClaimValue a -> b)
  -> (ClaimValue a -> b)
  -> Claim a
  -> b
claim f g = \case
  EssentialClaim cv -> f cv
  NonEssentialClaim cv -> g cv

claimValueFromClaim ::
  Claim a
  -> ClaimValue a
claimValueFromClaim = \case
  EssentialClaim cv -> cv
  NonEssentialClaim cv -> cv

setClaimValue ::
  Claim a
  -> ClaimValue a
  -> Claim a
setClaimValue c cv = case c of
  EssentialClaim _    -> EssentialClaim cv
  NonEssentialClaim _ -> NonEssentialClaim cv

class HasClaimValue (s :: * -> *) a where
  claimValueLens :: Lens' (s a) (ClaimValue a)

instance HasClaimValue Claim a where
  claimValueLens =
    lens claimValueFromClaim setClaimValue

instance ToJSON1 Claim where
  liftToJSON f g c =
    let
      o = claimValueToObject f g $ c ^. claimValueLens
      essential = Bool $ claim (const True) (const False) c
    in
      Object $ HM.insert "essential" essential o

instance Eq1 Claim where
  liftEq f c1 c2 =
    case (c1, c2) of
      (EssentialClaim _, NonEssentialClaim _)        -> False
      (NonEssentialClaim _, EssentialClaim _)        -> False
      (EssentialClaim cv1, EssentialClaim cv2)       -> liftEq f cv1 cv2
      (NonEssentialClaim cv1, NonEssentialClaim cv2) -> liftEq f cv1 cv2

instance Show1 Claim where
  liftShowsPrec f g n c s =
    let
      showValue con = (con <>) . ($ s) . liftShowsPrec f g n
    in
      claim (showValue "EssentialClaim ") (showValue "NonEssentialClaim ") c

instance FromJSON1 Claim where
  liftParseJSON f g =
    withObject "Claim" $ \o -> do
      essential <- o .: "essential"
      let c = bool NonEssentialClaim EssentialClaim essential
      c <$> liftParseJSON f g (Object o)

instance ToJSON a => ToJSON (Claim a) where
  toJSON = toJSON1
