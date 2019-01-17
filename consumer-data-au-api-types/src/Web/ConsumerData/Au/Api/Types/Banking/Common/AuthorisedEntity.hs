{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.AuthorisedEntity
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.AuthorisedEntity
  ) where

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Tag


data AuthorisedEntity = AuthorisedEntity
  { _authorisedEntityName                 :: Text
  , _authorisedEntityFinancialInstitution :: Text
  , _authorisedEntityAbn                  :: Maybe Abn
  , _authorisedEntityAcn                  :: Maybe Acn
  , _authorisedEntityArbn                 :: Maybe Arbn
  } deriving (Eq, Show)

authorisedEntityDecoder :: Monad f => Decoder f AuthorisedEntity
authorisedEntityDecoder =
  AuthorisedEntity
    <$> D.atKey "name" D.text
    <*> D.atKey "financialInstitution" D.text
    <*> atKeyOptional' "abn" abnDecoder
    <*> atKeyOptional' "acn" acnDecoder
    <*> atKeyOptional' "arbn" arbnDecoder

instance JsonDecode OB AuthorisedEntity where
  mkDecoder = tagOb authorisedEntityDecoder

authorisedEntityEncoder :: Applicative f => Encoder f AuthorisedEntity
authorisedEntityEncoder = E.mapLikeObj $ \ p ->
  E.atKey' "name" E.text (_authorisedEntityName p) .
  E.atKey' "financialInstitution" E.text (_authorisedEntityFinancialInstitution p) .
  maybeOrAbsentE "abn" abnEncoder (_authorisedEntityAbn p) .
  maybeOrAbsentE "acn" acnEncoder (_authorisedEntityAcn p) .
  maybeOrAbsentE "arbn" arbnEncoder (_authorisedEntityArbn p)

instance JsonEncode OB AuthorisedEntity where
  mkEncoder = tagOb authorisedEntityEncoder


data Abn =
  Abn { unAbn :: Text }
  deriving (Eq, Show)

abnDecoder :: Monad f => Decoder f Abn
abnDecoder = Abn <$> D.text

abnEncoder :: Applicative f => Encoder f Abn
abnEncoder = unAbn >$< E.text


data Acn =
  Acn { unAcn :: Text }
  deriving (Eq, Show)

acnDecoder :: Monad f => Decoder f Acn
acnDecoder = Acn <$> D.text

acnEncoder :: Applicative f => Encoder f Acn
acnEncoder = unAcn >$< E.text


data Arbn =
  Arbn { unArbn :: Text }
  deriving (Eq, Show)

arbnDecoder :: Monad f => Decoder f Arbn
arbnDecoder = Arbn <$> D.text

arbnEncoder :: Applicative f => Encoder f Arbn
arbnEncoder = unArbn >$< E.text
