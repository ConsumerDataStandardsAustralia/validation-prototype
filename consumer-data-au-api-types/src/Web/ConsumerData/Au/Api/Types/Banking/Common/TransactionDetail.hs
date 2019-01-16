{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionDetail
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionDetail
  ) where

import           Control.Lens               (Prism', prism, ( # ))
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import           Waargonaut.Encode          (Encoder')
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
    (Transaction, transactionDecoder, transactionMLO)
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers
import Web.ConsumerData.Au.Api.Types.Tag


newtype TransactionDetailResponse = TransactionDetailResponse
  { unTransactionDetail :: TransactionDetail }
  deriving (Eq, Show)

transactionDetailResponseDecoder :: Monad f => Decoder f TransactionDetailResponse
transactionDetailResponseDecoder = D.atKey "transaction" (TransactionDetailResponse <$> transactionDetailDecoder)

transactionDetailResponseEncoder :: Applicative f => Encoder f TransactionDetailResponse
transactionDetailResponseEncoder = E.mapLikeObj $ E.atKey' "transaction" transactionDetailEncoder . unTransactionDetail

instance JsonDecode OB TransactionDetailResponse where
  mkDecoder = tagOb transactionDetailResponseDecoder

instance JsonEncode OB TransactionDetailResponse where
  mkEncoder = tagOb transactionDetailResponseEncoder


data TransactionDetail = TransactionDetail
  { _transactionDetailTransaction  :: Transaction
  , _transactionDetailExtendedData :: TransactionExtendedData
  } deriving (Generic, Show, Eq)

transactionDetailDecoder :: Monad f => Decoder f TransactionDetail
transactionDetailDecoder =
  TransactionDetail
    <$> transactionDecoder
    <*> D.atKey "extendedData" transactionExtendedDataDecoder

transactionDetailEncoder ::  Applicative f => Encoder f TransactionDetail
transactionDetailEncoder = E.mapLikeObj $ \td ->
  transactionMLO (_transactionDetailTransaction td) .
  E.atKey' "extendedData" transactionExtendedDataEncoder (_transactionDetailExtendedData td)


data TransactionExtendedData = TransactionExtendedData
  { _transactionExtendedDataPayer         :: Maybe Text
  , _transactionExtendedDataPayee         :: Maybe Text
  , _transactionExtendedDataExtensionType :: Maybe TransactionExtendedDataExtensionType
  , _transactionExtendedDataService       :: TransactionExtendedDataService
  } deriving (Generic, Show, Eq)

transactionExtendedDataDecoder :: Monad f => Decoder f TransactionExtendedData
transactionExtendedDataDecoder =
  TransactionExtendedData
    <$> atKeyOptional' "payer" D.text
    <*> atKeyOptional' "payee" D.text
    <*> D.maybeOrNull extensionTypeDecoder
    <*> D.atKey "serviceId" transactionExtendedDataServiceDecoder

transactionExtendedDataEncoder :: Encoder' TransactionExtendedData
transactionExtendedDataEncoder = E.mapLikeObj $ \(TransactionExtendedData payer payee et serv) ->
  maybeOrAbsentE "payer" E.text payer .
  maybeOrAbsentE "payee" E.text payee .
  maybe id extensionTypeFields et .
  E.atKey' "serviceId" transactionExtendedDataServiceEncoder serv


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
