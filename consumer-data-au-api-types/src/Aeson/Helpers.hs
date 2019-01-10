{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aeson.Helpers where

import Control.Lens     (Prism', prism', (^?))
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, parseJSON)
import Data.Set         as Set
import Data.Text        as T
import Text.URI         as URI

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

newtype SpaceSeparatedSet = SpaceSeparatedSet
  {
    fromSpaceSeparatedSet :: Set T.Text
  } deriving (Show)

instance ToJSON SpaceSeparatedSet where
  toJSON (SpaceSeparatedSet s) = toJSON . T.intercalate " " . Set.toList $ s

instance FromJSON SpaceSeparatedSet where
  parseJSON v = SpaceSeparatedSet . Set.fromList .  T.split (== ' ') <$> parseJSON v

parseSpaceSeparatedSet :: Ord a => Prism' Text a -> String -> Value -> Parser (Set a)
parseSpaceSeparatedSet p n = fmap Set.fromList . (>>= traverse (parseWithPrism p n)) . fmap (Set.toList . fromSpaceSeparatedSet) .  parseJSON

toJsonSpaceSeparatedSet :: (a->T.Text) -> Set a -> Value
toJsonSpaceSeparatedSet f = toJSON . SpaceSeparatedSet . Set.map f

parseUri :: Value -> Parser URI
parseUri = (>>= toParser) . fmap mkURI . parseJSON
    where
      toParser =
        either (fail . show) pure

instance ToJSON URI where
  toJSON = toJSON . URI.render

instance FromJSON URI where
  parseJSON = parseUri

_URI :: Prism' T.Text URI
_URI = prism' URI.render mkURI

