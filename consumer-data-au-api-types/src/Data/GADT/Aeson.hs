{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Data.GADT.Aeson where

import           Data.Aeson          (Value (Object), withObject, (.:?))
import           Data.Aeson.Types    (Parser)
import           Data.Bool           (bool)
import           Data.Dependent.Map
    (DMap, DSum (..), GCompare (..), empty, insert, toList)
import qualified Data.HashMap.Strict as HM
import           Data.Monoid         (First (First, getFirst))
import           Data.Some           (Some (This))
import           Data.Text           (Text)
import           GHC.Prim            (Proxy#, proxy#)
import           GHC.TypeLits        (KnownSymbol, symbolVal')

class FromJSONViaKey k f where
  parseJSONViaKey ::  k a -> Value -> Parser (f a)

class ToJSONViaKey k f where
  toJSONViaKey :: k a -> f a -> Value

class JSONKey k where
  toFieldName :: k a -> Text
  fromFieldName :: Text -> Maybe (Some k)
  keys :: [Some k]
  {-# MINIMAL toFieldName, keys #-}

  -- | This is not optimal as it does a linear search through 'keys' to find the first matching field.
  fromFieldName = defaultFromFieldName

defaultFromFieldName
  :: JSONKey tag
  => Text
  -> Maybe (Some tag)
defaultFromFieldName t =
  getFirst $ foldMap (\(This k) -> First $ bool Nothing (Just (This k)) (toFieldName k == t)) keys

toObjectDMap ::
  ( JSONKey k
  , ToJSONViaKey k f
  )
  => DMap k f
  -> HM.HashMap Text Value
toObjectDMap dm =
    let
      toPair (k :=> v) = (toFieldName k, toJSONViaKey k v)
    in
      HM.fromList . fmap toPair . toList $ dm

toJSONDMap ::
  ( JSONKey k
  , ToJSONViaKey k f
  )
  => DMap k f
  -> Value
toJSONDMap =
  Object . toObjectDMap

-- | 'parseJSON' implementation for a 'DMap'. The reason we don't provide the instance itself is so
-- a name for the object can be provided, which gives better error messages.
mkParseJSON
  :: forall k f.
    (JSONKey k, GCompare k, FromJSONViaKey k f)
  => String
  -> Value
  -> Parser (DMap k f)
mkParseJSON t = withObject t $ \o ->
  let
    add :: Some k -> Parser (DMap k f) -> Parser (DMap k f)
    add (This k) pDm = do
      mv <- o .:? toFieldName k
      ma <- maybe (pure Nothing) (fmap Just . parseJSONViaKey k) mv
      maybe id (insert k) ma <$> pDm
  in
    foldr add (pure empty) keys

symName
  :: forall name. KnownSymbol name => String
symName =
  symbolVal' (proxy# :: (Proxy# name))
