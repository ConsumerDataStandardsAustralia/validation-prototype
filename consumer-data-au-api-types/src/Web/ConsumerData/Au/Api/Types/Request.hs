{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.ConsumerData.Au.Api.Types.Request where

import           Data.Functor.Identity      (Identity)
import           Data.Proxy                 (Proxy (..))
import           Data.Tagged                (Tagged, tagWith, untag)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Tag


type StandardRequest = Request Meta

data Request meta dada = Request
  { _requestData  :: dada
  , _requestMeta  :: meta
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable)


instance
  (JsonDecode OB d, JsonDecode OB m)
  => JsonDecode OB (Request m d)
  where
  mkDecoder :: forall f. Monad f => Tagged OB (Decoder f (Request m d))
  mkDecoder = tagWith (Proxy :: Proxy OB) $ requestDecoder
    (untag (mkDecoder :: Tagged OB (Decoder f m)))
    (untag (mkDecoder :: Tagged OB (Decoder f d)))


instance
  (JsonEncode OB d, JsonEncode OB m)
  => JsonEncode OB (Request m d)
  where
  mkEncoder :: forall f. Applicative f => Tagged OB (Encoder f (Request m d))
  mkEncoder = tagWith (Proxy :: Proxy OB) $ requestEncoder
    (untag (mkEncoder :: Tagged OB (Encoder Identity m)))
    (untag (mkEncoder :: Tagged OB (Encoder Identity d)))

requestDecoder
  :: Monad f
  => Decoder f m
  -> Decoder f d
  -> Decoder f (Request m d)
requestDecoder md dd =
  Request
    <$> D.atKey "data" dd
    <*> D.atKey "meta" md

requestEncoder
  :: Applicative f
  => Encoder Identity m
  -> Encoder Identity d
  -> Encoder f (Request m d)
requestEncoder me de = E.mapLikeObj $ \r ->
  E.atKey' "data" de (_requestData r) .
  E.atKey' "meta" me (_requestMeta r)


data Meta = Meta
  deriving (Eq, Show)

instance JsonDecode OB Meta where
  mkDecoder = tagWith (Proxy :: Proxy OB) . D.withCursor . const $ pure Meta

instance JsonEncode OB Meta where
  mkEncoder = tagWith (Proxy :: Proxy OB) . E.mapLikeObj $ flip const

mkStandardRequest :: d -> Request Meta d
mkStandardRequest d = Request d Meta
