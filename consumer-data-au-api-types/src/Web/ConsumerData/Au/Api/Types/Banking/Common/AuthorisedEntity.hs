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

import Web.ConsumerData.Au.Api.Types.Tag


-- | Information on the authorised entity that is able to perform a direct debit <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaauthorisedentity CDR AU v0.1.0 AuthorisedEntity>
data AuthorisedEntity = AuthorisedEntity
  { _authorisedEntityName                 :: Text -- ^ Name of the authorised entity.
  , _authorisedEntityFinancialInstitution :: Text -- ^ Name of the financial institution through which the direct debit will be executed.
-- WARNING website name mistyped `financialInsitution`
  , _authorisedEntityAbn                  :: Maybe Abn -- ^ Australian Business Number for the authorised entity.
  , _authorisedEntityAcn                  :: Maybe Acn -- ^ Australian Company Number for the authorised entity.
  } deriving (Eq, Show)

authorisedEntityDecoder :: Monad f => Decoder f AuthorisedEntity
authorisedEntityDecoder = D.withCursor $ \c -> do
  o <- D.down c
  name <- D.fromKey "name" D.text o
  finInst <- D.fromKey "financialInstitution" D.text o -- WARNING
  abn <- D.try $ D.fromKey "abn" abnDecoder o
  acn <- D.try $ D.fromKey "acn" acnDecoder o
  pure $ AuthorisedEntity name finInst abn acn

instance JsonDecode OB AuthorisedEntity where
  mkDecoder = tagOb authorisedEntityDecoder

authorisedEntityEncoder :: Applicative f => Encoder f AuthorisedEntity
authorisedEntityEncoder = E.mapLikeObj $ \ p ->
  E.atKey' "name" E.text (_authorisedEntityName p) .
  E.atKey' "financialInstitution" E.text (_authorisedEntityFinancialInstitution p) .
  E.atKey' "abn" (E.maybeOrNull abnEncoder) (_authorisedEntityAbn p) .
  E.atKey' "acn" (E.maybeOrNull acnEncoder) (_authorisedEntityAcn p)

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
