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

newtype SpaceSeperatedSet = SpaceSeperatedSet
  {
    fromSpaceSeperatedSet :: Set T.Text
  } deriving (Show)

instance ToJSON SpaceSeperatedSet where
  toJSON (SpaceSeperatedSet s) = toJSON . T.intercalate " " . Set.toList $ s

instance FromJSON SpaceSeperatedSet where
  parseJSON v = SpaceSeperatedSet . Set.fromList .  T.split (== ' ') <$> parseJSON v

parseSpaceSeperatedSet :: Ord a => Prism' Text a -> String -> Value -> Parser (Set a)
parseSpaceSeperatedSet p n = fmap Set.fromList . (>>= traverse (parseWithPrism p n)) . fmap (Set.toList . fromSpaceSeperatedSet) .  parseJSON

toJsonSpaceSeperatedSet :: (a->T.Text) -> Set a -> Value
toJsonSpaceSeperatedSet f = toJSON . SpaceSeperatedSet . Set.map f

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
