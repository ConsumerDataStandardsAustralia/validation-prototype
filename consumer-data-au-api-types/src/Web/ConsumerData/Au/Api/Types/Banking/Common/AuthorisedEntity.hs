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


-- | Information on the authorised entity that is able to perform a direct debit <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaauthorisedentity CDR AU v0.1.0 AuthorisedEntity>
data AuthorisedEntity = AuthorisedEntity
  { _authorisedEntityName                 :: Text -- ^ Name of the authorised entity.
  , _authorisedEntityFinancialInstitution :: Text -- ^ Name of the financial institution through which the direct debit will be executed.
  , _authorisedEntityAbn                  :: Maybe Abn -- ^ Australian Business Number for the authorised entity.
  , _authorisedEntityAcn                  :: Maybe Acn -- ^ Australian Company Number for the authorised entity.
  , _authorisedEntityArbn                 :: Maybe Arbn
  } deriving (Eq, Show)

authorisedEntityDecoder :: Monad f => Decoder f AuthorisedEntity
authorisedEntityDecoder =
  AuthorisedEntity
    <$> D.atKey "name" D.text
    <*> D.atKey "financialInstitution" D.text -- WARNING miss typed in swagger `financialInsitution`
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


-- | Australian Business Number. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaabn CDR AU v0.1.0 ABN>
data Abn =
  Abn { unAbn :: Text }
  deriving (Eq, Show)

abnDecoder :: Monad f => Decoder f Abn
abnDecoder = Abn <$> D.text

abnEncoder :: Applicative f => Encoder f Abn
abnEncoder = unAbn >$< E.text


-- | Australian Company Number. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaacn CDR AU v0.1.0 ABN>
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
