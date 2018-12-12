{-# LANGUAGE RankNTypes #-}
module Aeson.Helpers where

import Control.Lens     (Prism', (^?))
import Data.Aeson.Types (FromJSON (..), Parser, Value, parseJSON)
import Data.Text        (Text)

parseJSONWithPrism ::
  ( FromJSON s
  , Show s
  )
  => Prism' s a
  -> String
  -> Value
  -> Parser a
parseJSONWithPrism p name v = do
    t <- parseJSON v
    parseWithPrism  p name t

parseWithPrism ::
  ( FromJSON s
  , Show s
  )
  => Prism' s a
  -> String
  -> s
  -> Parser a
parseWithPrism p name t = maybe (fail $ show t <> " is not a " <> name) pure (t ^? p)
