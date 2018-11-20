{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Web.ConsumerData.Au.Api.Types.SumTypeHelpers where

import           Control.Lens             (At, Index, IxValue)
import           Control.Monad.Error.Lens (throwing)
import           Data.Maybe               (maybe)
import           Data.Text                (Text)
import           Waargonaut.Decode        (Decoder)
import qualified Waargonaut.Decode        as D
import           Waargonaut.Decode.Error  (_ConversionFailure)
import qualified Waargonaut.Encode        as E
import           Waargonaut.Types         (Json)

typeTaggedField
  :: (At c, (Index c) ~ Text, IxValue c ~ Json)
  => Index c
  -> Index c
  -> E.Encoder' a
  -> a
  -> c
  -> c
typeTaggedField tKey tVal dataE d = E.atKey' tKey E.text tVal . E.atKey' tVal dataE d

data TypedTagField f a where
  TypedTagField :: forall a b f. (b -> a) -> Decoder f b -> TypedTagField f a

typeTaggedDecoder :: Monad f => Text -> (Text -> Maybe (TypedTagField f a)) -> Decoder f a
typeTaggedDecoder tKey f = D.withCursor $ \c -> do
  o <- D.down c
  t <- D.fromKey tKey D.text o
  maybe
    (throwing _ConversionFailure $ "Invalid " <> tKey <> "type: ")
    (\(TypedTagField cons d) -> cons <$> (D.fromKey t d o))
    (f t)
