{-# LANGUAGE CPP #-}

module Data.Time.Waargonaut where

import           Control.Monad (ap, liftM)
import           Control.Monad.Except (throwError)
import           Control.Monad.Fail (MonadFail (fail))
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Format
import           Waargonaut.Decode (Decoder)
import           Waargonaut.Encode (Encoder)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D
import qualified Waargonaut.Encode as E

utcTimeFormatString :: String
utcTimeFormatString = "%FT%T%QZ"

utcTimeDecoder :: Monad f => Decoder f UTCTime
utcTimeDecoder = D.withCursor $ \c -> D.focus D.string c >>=
    foldResult (throwError . D.ConversionFailure . T.pack) pure .
    parseTimeM True defaultTimeLocale utcTimeFormatString

utcTimeEncoder :: Applicative f => Encoder f UTCTime
utcTimeEncoder = T.pack . formatTime defaultTimeLocale utcTimeFormatString >$< E.text

-- | A 'MonadFail' that gives you access to the failure string
data Result a = Fail String | Win a deriving (Show, Eq)

instance Functor Result where
  fmap = liftM

instance Applicative Result where
  pure = Win
  (<*>) = ap

instance Monad Result where
  Fail s >>= _ = Fail s
  Win  a >>= f = f a
#if ! MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
  fail = Fail
#endif

instance MonadFail Result where
  fail = Fail

foldResult :: (String -> b) -> (a -> b) -> Result a -> b
foldResult sb ab r =
  case r of
    Fail s -> sb s
    Win a -> ab a
