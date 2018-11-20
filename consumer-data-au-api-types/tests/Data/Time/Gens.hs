module Data.Time.Gens where

import           Data.Time
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

utcTimeGen :: Gen UTCTime
utcTimeGen = UTCTime <$> dayGen <*> diffTimeGen

dayGen :: Gen Day
dayGen = ModifiedJulianDay <$> Gen.integral (Range.linear 0 100000)

diffTimeGen :: Gen DiffTime
diffTimeGen =
  Gen.frequency [(9,secondGen), (1,picoGen)]
  where
    secondGen = secondsToDiffTime <$> Gen.integral (Range.linear 0 86400)
    picoGen = picosecondsToDiffTime <$> Gen.integral (Range.linear 0 (mkPico 86401 - 1)) -- 0 <= t < 86401
    mkPico :: Integer -> Integer
    mkPico = (* 10 ^ (12 :: Integer))
