{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.ConsumerData.Au.Api.Types.Response where

import Control.Lens

import Control.Error (note)
import           Control.Lens.TH            (makeLenses, makeWrapped)
import           Control.Monad              ((<=<))
import           Control.Monad.Error.Lens   (throwing)
import           Data.Bifoldable            (Bifoldable (..))
import           Data.Bifunctor             (Bifunctor (..))
import           Data.Bitraversable
    (Bitraversable (..), bifoldMapDefault, bimapDefault)
import           Data.Bool                  (bool)
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity      (Identity)
import           Data.List.NonEmpty         (nonEmpty)
import           Data.Maybe                 (fromJust)
import           Data.Number.Nat            (Nat, fromNat, toNat)
import           Data.Number.Nat1           (Nat1, fromNat1, toNat1)
import           Data.Proxy                 (Proxy (..))
import           Data.Tagged                (Tagged, tagWith, untag)
import           Data.Text                  (Text, pack)
import           Data.Text.Lens             (_Text)
import           GHC.Generics               (Generic)
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseQueryParam, toQueryParam)
import           Servant.Links              (Link, linkURI)
import           Text.URI
    (Authority, RText, RTextLabel (PathPiece, Scheme),
    URI (uriAuthority, uriPath, uriScheme), mkURI, render)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (_ConversionFailure)
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import Waargonaut.Helpers                   (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Tag

eitherDecoder :: Monad m => Either Text a -> Decoder m a
eitherDecoder e =
  D.withCursor . const $
    either
     (throwing _ConversionFailure)
     pure
     e

intToNat :: Int -> Either Text Nat
intToNat n = bool (Left "Negative number") (pure $ toNat n) (n >= 0)

natDecoder :: Monad m => Decoder m Nat
natDecoder = D.int >>= (eitherDecoder . intToNat)

natEncoder :: Applicative f => Encoder f Nat
natEncoder = fromNat >$< E.int

intToNat1 :: Int -> Either Text Nat1
intToNat1 n = bool (Left "Negative or zero number") (Right $ toNat1 n) (n > 0)

nat1Decoder :: Monad m => Decoder m Nat1
nat1Decoder = D.int >>= (eitherDecoder . intToNat1)

nat1Encoder :: Applicative f => Encoder f Nat1
nat1Encoder = fromNat1 >$< E.int

uriEncoder :: Applicative f => Encoder f URI
uriEncoder = render >$< E.text

uriDecoder :: Monad f => Decoder f URI
uriDecoder = D.text >>= (\t -> eitherDecoder . note ("Not a valid URI: " <> t) . mkURI $ t)

type StandardResponse = Response MetaStandard LinksStandard
type PaginatedResponse = Response MetaPaginated LinksPaginated

data Response meta links dada = Response
  { _responseData  :: dada
  , _responseLinks :: links
  , _responseMeta  :: meta
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Bifunctor (Response meta) where
  bimap = bimapDefault

instance Bifoldable (Response meta) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (Response meta) where
  bitraverse f g (Response d l m) = Response <$> g d <*> f l <*> pure m

instance
  (JsonDecode OB d, JsonDecode OB m, JsonDecode OB l)
  => JsonDecode OB (Response m l d)
  where
  mkDecoder :: forall f. Monad f => Tagged OB (Decoder f (Response m l d))
  mkDecoder = tagWith (Proxy :: Proxy OB) $ responseDecoder
    (untag (mkDecoder :: Tagged OB (Decoder f m)))
    (untag (mkDecoder :: Tagged OB (Decoder f l)))
    (untag (mkDecoder :: Tagged OB (Decoder f d)))


instance
  (JsonEncode OB d, JsonEncode OB m, JsonEncode OB l)
  => JsonEncode OB (Response m l d)
  where
  mkEncoder :: forall f. Applicative f => Tagged OB (Encoder f (Response m l d))
  mkEncoder = tagWith (Proxy :: Proxy OB) $ responseEncoder
    (untag (mkEncoder :: Tagged OB (Encoder Identity m)))
    (untag (mkEncoder :: Tagged OB (Encoder Identity l)))
    (untag (mkEncoder :: Tagged OB (Encoder Identity d)))

responseEncoder
  :: Applicative f
  => Encoder Identity m
  -> Encoder Identity l
  -> Encoder Identity d
  -> Encoder f (Response m l d)
responseEncoder me le de = E.mapLikeObj $ \r ->
  E.atKey' "data" de (_responseData r) .
  E.atKey' "links" le (_responseLinks r) .
  E.atKey' "meta" me (_responseMeta r)

responseDecoder
  :: Monad f
  => Decoder f m
  -> Decoder f l
  -> Decoder f d
  -> Decoder f (Response m l d)
responseDecoder md ld dd = D.withCursor $ \c -> do
  o <- D.down c
  Response
    <$> D.fromKey "data" dd o
    <*> D.fromKey "links" ld o
    <*> D.fromKey "meta" md o

data MetaStandard = MetaStandard deriving (Eq, Show)

instance JsonDecode OB MetaStandard where
  mkDecoder = tagWith (Proxy :: Proxy OB) . D.withCursor . const $ pure MetaStandard


instance JsonEncode OB MetaStandard where
  mkEncoder = tagWith (Proxy :: Proxy OB) . E.mapLikeObj $ flip const

data LinksStandard = LinksStandard
  { _linksStandardSelf :: URI }
  deriving (Eq, Ord, Show)

linksStandardDecoder :: Monad f => Decoder f LinksStandard
linksStandardDecoder = D.withCursor $ \c -> do
  o <- D.down c
  LinksStandard
    <$> D.fromKey "self" uriDecoder o

linksStandardEncoder :: Applicative f => Encoder f LinksStandard
linksStandardEncoder = E.mapLikeObj $ \r ->
  E.atKey' "self" uriEncoder (_linksStandardSelf r)


instance JsonDecode OB LinksStandard where
  mkDecoder = tagOb $ linksStandardDecoder


instance JsonEncode OB LinksStandard where
  mkEncoder = tagOb $ linksStandardEncoder

data MetaPaginated = MetaPaginated
  { _metaPaginatedTotalRecords :: Nat
  , _metaPaginatedTotalPages   :: Nat1
  }
  deriving (Eq, Ord, Show)

metaPaginatedDecoder :: Monad f => Decoder f MetaPaginated
metaPaginatedDecoder = D.withCursor $ \c -> do
  o <- D.down c
  MetaPaginated
    <$> D.fromKey "totalRecords" natDecoder o
    <*> D.fromKey "totalPages" nat1Decoder o

metaPaginatedEncoder :: Applicative f => Encoder f MetaPaginated
metaPaginatedEncoder = E.mapLikeObj $ \r ->
  E.atKey' "totalRecords" natEncoder (_metaPaginatedTotalRecords r) .
  E.atKey' "totalPages" nat1Encoder (_metaPaginatedTotalPages r)

instance JsonDecode OB MetaPaginated where
  mkDecoder = tagOb $ metaPaginatedDecoder

instance JsonEncode OB MetaPaginated where
  mkEncoder = tagOb $ metaPaginatedEncoder

data LinksPaginated = LinksPaginated
  { _linksPaginatedSelf  :: URI
  , _linksPaginatedFirst :: Maybe URI
  , _linksPaginatedPrev  :: Maybe URI
  , _linksPaginatedNext  :: Maybe URI
  , _linksPaginatedLast  :: Maybe URI
  }
  deriving (Eq, Ord, Show)

linksPaginatedDecoder :: Monad f => Decoder f LinksPaginated
linksPaginatedDecoder =
  LinksPaginated
    <$> D.atKey "self" uriDecoder
    <*> atKeyOptional' "first" uriDecoder
    <*> atKeyOptional' "prev" uriDecoder
    <*> atKeyOptional' "next" uriDecoder
    <*> atKeyOptional' "last" uriDecoder

linksPaginatedEncoder :: Applicative f => Encoder f LinksPaginated
linksPaginatedEncoder = E.mapLikeObj $ \r ->
  E.atKey' "self" uriEncoder (_linksPaginatedSelf r) .
  maybeOrAbsentE "first" uriEncoder (_linksPaginatedFirst r) .
  maybeOrAbsentE "prev" uriEncoder (_linksPaginatedPrev r) .
  maybeOrAbsentE "next" uriEncoder (_linksPaginatedNext r) .
  maybeOrAbsentE "last" uriEncoder (_linksPaginatedLast r)

instance JsonDecode OB LinksPaginated where
  mkDecoder = tagOb $ linksPaginatedDecoder

instance JsonEncode OB LinksPaginated where
  mkEncoder = tagOb $ linksPaginatedEncoder

newtype PageNumber = PageNumber Nat1
makeWrapped ''PageNumber


instance ToHttpApiData PageNumber where
  toQueryParam = (^. _Wrapped . to show . from _Text)

instance FromHttpApiData PageNumber where
  parseQueryParam = fmap PageNumber . (intToNat1 <=< parseQueryParam)

data Paginator = Paginator
  { _paginatorCurrentPage  :: PageNumber
  , _paginatorLastPage     :: PageNumber
  , _paginatorTotalRecords :: Nat
  , _paginatorMkLink       :: PageNumber -> Link
  }
makeLenses ''Paginator


data LinkQualifier = LinkQualifier
  { _linkQualifierScheme     :: RText 'Scheme
  , _linkQualifierAuthority  :: Authority
  , _linkQualifierPathPrefix :: [RText 'PathPiece]
  }
makeLenses ''LinkQualifier

fqUri :: LinkQualifier -> URI -> URI
fqUri q u = u
  { uriScheme    = Just $ q ^. linkQualifierScheme
  , uriAuthority = Right $ q ^. linkQualifierAuthority
  , uriPath      = maybe thisPath mergePrefix nonEmptyPrefix
  }
  where
    nonEmptyPrefix     = q ^. linkQualifierPathPrefix . to nonEmpty
    thisPath           = uriPath u
    mergePrefix preNel = Just $ maybe (False, preNel) ((preNel <>) <$>) thisPath

-- TODO: Probably going to hell for writing this. But servant shouldn't return
-- a bogus URI, surely...
linkToUri :: LinkQualifier -> Link -> URI
linkToUri q = fqUri q . fromJust . mkURI . pack . show . linkURI

mkStandardResponse :: d -> LinkQualifier -> Link -> Response MetaStandard LinksStandard d
mkStandardResponse d q l = Response d (LinksStandard $ linkToUri q l) MetaStandard

mkPaginatedResponse :: d -> LinkQualifier -> Paginator -> Response MetaPaginated LinksPaginated d
mkPaginatedResponse d q p = Response d
  (LinksPaginated selfU firstU prevU nextU lastU)
  (MetaPaginated
   (p ^. paginatorTotalRecords)
   (p ^. paginatorLastPage . _Wrapped)
  )
  where
    selfU = linkToUri q $ (p^.paginatorMkLink) (p^.paginatorCurrentPage)
    cp = p^.paginatorCurrentPage._Wrapped
    lp = p^.paginatorLastPage._Wrapped
    ml = p^.paginatorMkLink
    onFirst = cp == 1
    onLast = cp == lp
    firstU = bool (Just . linkToUri q $ ml (PageNumber 1)) Nothing onFirst
    prevU = bool (Just . linkToUri q $ ml (PageNumber (cp - 1))) Nothing onFirst
    nextU = bool (Just . linkToUri q $ ml (PageNumber (cp + 1))) Nothing onLast
    lastU = bool (Just . linkToUri q $ ml (PageNumber lp)) Nothing onLast
