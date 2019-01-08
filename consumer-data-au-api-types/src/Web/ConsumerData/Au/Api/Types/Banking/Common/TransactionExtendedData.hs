{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionExtendedData
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionExtendedData
  ) where

import           Control.Lens               (Prism', prism, ( # ))
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder')
import qualified Waargonaut.Encode          as E
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers


-- Contains more detailed information specific to transactions originated via NPP. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaextendedtransactiondata CDR AU v0.1.0 TransactionExtendedData>
data TransactionExtendedData = TransactionExtendedData
  { _transactionExtendedDataPayer         :: Maybe Text -- ^ Label of the originating payer. Mandatory for an inbound payment.
  , _transactionExtendedDataPayee         :: Maybe Text -- ^ Label of the target PayID. Mandatory for an outbound payment.
  , _transactionExtendedDataExtensionType :: Maybe TransactionExtendedDataExtensionType -- ^ The type of transaction data extension.
  , _transactionExtendedDataService       :: TransactionExtendedDataService -- ^ Identifier of the applicable overlay service.
  } deriving (Generic, Show, Eq)

transactionExtendedDataDecoder :: Monad f => Decoder f TransactionExtendedData
transactionExtendedDataDecoder =
  TransactionExtendedData
    <$> atKeyOptional' "payer" D.text
    <*> atKeyOptional' "payee" D.text
    <*> D.maybeOrNull extensionTypeDecoder
    <*> D.atKey "service" transactionExtendedDataServiceDecoder

transactionExtendedDataEncoder :: Encoder' TransactionExtendedData
transactionExtendedDataEncoder = E.mapLikeObj $ \(TransactionExtendedData payer payee et serv) ->
  maybeOrAbsentE "payer" E.text payer .
  maybeOrAbsentE "payee" E.text payee .
  maybe id extensionTypeFields et .
  E.atKey' "service" transactionExtendedDataServiceEncoder serv


data TransactionExtendedDataExtensionType =
    TEDExtendedDescription Text
  deriving (Eq, Show)

extensionTypeDecoder :: Monad f => Decoder f TransactionExtendedDataExtensionType
extensionTypeDecoder = typeTaggedDecoder "extensionUType" $ \case
  "extendedDescription" -> Just $ (TypedTagField TEDExtendedDescription D.text)
  _                     -> Nothing

extensionTypeFields ::
  (Monoid ws, Semigroup ws)
  => TransactionExtendedDataExtensionType -> MapLikeObj ws Json -> MapLikeObj ws Json
extensionTypeFields = \case
  TEDExtendedDescription t -> fields "extendedDescription" E.text t
  where
    fields = typeTaggedField "extensionUType"


data TransactionExtendedDataService =
  X2P101
  deriving (Bounded, Enum, Eq, Ord, Show)

transactionExtendedDataServiceText ::
  Prism' Text TransactionExtendedDataService
transactionExtendedDataServiceText =
  prism (\case
          X2P101 -> "X2P1.01"
      )
      (\case
          "X2P1.01" -> Right X2P101
          t -> Left t
      )

transactionExtendedDataServiceDecoder ::
  Monad f => Decoder f TransactionExtendedDataService
transactionExtendedDataServiceDecoder =
  D.prismDOrFail (D.ConversionFailure "TransactionExtendedDataService")
    transactionExtendedDataServiceText D.text

transactionExtendedDataServiceEncoder ::
  Encoder' TransactionExtendedDataService
transactionExtendedDataServiceEncoder =
  (transactionExtendedDataServiceText #) >$< E.text'
