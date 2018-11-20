{-# LANGUAGE MultiParamTypeClasses #-}
module Web.ConsumerData.Au.Api.Types.Stub where

import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import Web.ConsumerData.Au.Api.Types.Tag

data Stub = Stub deriving (Eq, Show)

emptyObjDecoder :: Monad m => a -> D.Decoder m a
emptyObjDecoder = pure
emptyObjEncoder :: Applicative f => E.Encoder f a
emptyObjEncoder = E.mapLikeObj $ flip const

instance JsonDecode OB Stub where
  mkDecoder = tagOb $ emptyObjDecoder Stub


instance JsonEncode OB Stub where
  mkEncoder = tagOb emptyObjEncoder
