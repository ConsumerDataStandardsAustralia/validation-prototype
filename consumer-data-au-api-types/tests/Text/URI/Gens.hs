{-# LANGUAGE DataKinds #-}

module Text.URI.Gens where

import           Control.Monad       (join)
import           Control.Monad.Catch (MonadThrow)
import           Data.List.NonEmpty  (NonEmpty)
import qualified Data.List.NonEmpty  as NE
import           Data.Text           as T
import           Text.URI
    (Authority (Authority), RText, RTextLabel (Host, PathPiece, Scheme),
    URI (URI), mkHost, mkPathPiece, mkScheme)

import           Hedgehog       (MonadGen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

-- | Unreserved chars taken from <https://www.ietf.org/rfc/rfc3986.txt §2.3 RFC3986>
genUri ::
  ( MonadGen n
  , MonadThrow n
  )
  => n URI
genUri =
    URI
    <$> (Just <$> genScheme)
    <*> (Right <$> genAuthority)
    <*> genPathPieces
    <*> pure []
    <*> pure Nothing

genAuthority ::
  ( MonadGen n
  , MonadThrow n
  )
  => n Authority
genAuthority = Authority Nothing <$> genHost <*> genPort where
    genPort = Gen.maybe (Gen.word (Range.linear 1 65535))

-- | <https://tools.ietf.org/html/rfc3986#section-3.2.2 §3.2.2 of RFC3986> allows for much more than
-- this. We're not supporting IPs, and 'mkHost' doesn't support anything other than 'Gen.alphaNum' in
-- @regname@s
genHost ::
  ( MonadGen n
  , MonadThrow n
  )
  => n (RText 'Host)
genHost =
  mkHost =<< Gen.text (Range.linear 1 30) Gen.alphaNum

genPathPieces ::
  ( MonadGen n
  , MonadThrow n
  )
  => n (Maybe (Bool, NonEmpty (RText 'PathPiece)))
genPathPieces =
  let
    genPiece = mconcat <$> Gen.list (Range.linear 1 10) genPchar
    subsequentPieces = Gen.list (Range.linear 0 5) genPiece
    allThePieces = (NE.:|) <$> genPiece <*> subsequentPieces
    allTheTypedPieces = join . fmap (traverse mkPathPiece) $ allThePieces
    tuplé = (,) <$> Gen.bool <*> allTheTypedPieces
  in
    Gen.maybe tuplé

genScheme ::
  ( MonadGen n
  , MonadThrow n
  )
  => n (RText 'Scheme)
genScheme =
  let
    gsc = Gen.frequency [(9, Gen.alphaNum), (1, Gen.element "+-.")]
  in
    (mkScheme =<<) $ T.cons <$> Gen.lower <*> Gen.text (Range.linear 1 10) gsc

-- It says char, but it has to be @Text@ because percent encoded characters are 3 chars long.
genPchar ::
  MonadGen n
  => n T.Text
genPchar =
  Gen.frequency
    [ (85, T.singleton <$> genUnreserved)
    , (5, genPctEnc)
    , (5, T.singleton <$> genSubDelim)
    , (5, T.singleton <$> Gen.element ":@")
    ]

genUnreserved, genSubDelim ::
  MonadGen n
  => n Char

genUnreserved =
  Gen.frequency
    [ (90, Gen.lower)
    , (8, Gen.digit)
    , (2, Gen.element "-._~")
    ]

genSubDelim =
  Gen.element "!$&'()*+,;="

genPctEnc ::
  MonadGen n
  => n T.Text
genPctEnc =
  T.cons '%' <$> Gen.text (Range.singleton 2) Gen.hexit
