{-# LANGUAGE FlexibleContexts,TypeFamilies #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
  ) where

import           Data.Text          (pack)
import qualified Waargonaut.Decode  as D

-- | Helper decoder function from @additionalValue@
additionalValueDecoder :: Monad f => (D.Decoder f a) -> D.JCurs -> D.DecodeResult f a
additionalValueDecoder = D.fromKey $ pack "additionalValue"
