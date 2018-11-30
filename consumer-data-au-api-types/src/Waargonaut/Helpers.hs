module Waargonaut.Helpers where

import Control.Monad (join)
import Data.Text (Text)

import Waargonaut.Decode (Decoder, DecodeResult, JCurs)
import Waargonaut.Encode (Encoder')
import Waargonaut.Types (Json, MapLikeObj)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Encode as E

fromKeyOptional' :: Monad f => Text -> Decoder f a -> JCurs -> DecodeResult f (Maybe a)
fromKeyOptional' t d = fmap join . D.fromKeyOptional t (D.maybeOrNull d)

atKeyOptional' :: Monad f => Text -> Decoder f a -> Decoder f (Maybe a)
atKeyOptional' t d = join <$> D.atKeyOptional t (D.maybeOrNull d)

maybeOrAbsentE :: (Monoid ws, Semigroup ws) => Text -> Encoder' a -> Maybe a -> MapLikeObj ws Json -> MapLikeObj ws Json
maybeOrAbsentE t e = maybe id (E.atKey' t e)
