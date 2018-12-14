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

import Waargonaut.Helpers (atKeyOptional', maybeOrAbsentE)


-- Contains more detailed information specific to transactions originated via NPP. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaextendedtransactiondata CDR AU v0.1.0 TransactionExtendedData>
data TransactionExtendedData = TransactionExtendedData
  { _transactionExtendedDataPayer :: Maybe Text -- ^ Label of the originating payer. Mandatory for an inbound payment.
  , _transactionExtendedDataPayee :: Maybe Text -- ^ Label of the target PayID. Mandatory for an outbound payment.
  , _transactionExtendedDataExtensionType :: Maybe TransactionExtendedDataExtensionType -- ^ The type of transaction data extension.
  , _transactionExtendedDataExtendedDescription :: Maybe TransactionExtendedDataExtendedDescription -- ^ An extended string description. Only present if specified by the extension$type field.
  , _transactionExtendedDataService :: TransactionExtendedDataService -- ^ Identifier of the applicable overlay service.
  } deriving (Generic, Show, Eq)

transactionExtendedDataDecoder :: Monad f => Decoder f TransactionExtendedData
transactionExtendedDataDecoder =
  TransactionExtendedData
    <$> atKeyOptional' "payer" D.text
    <*> atKeyOptional' "payee" D.text
    <*> atKeyOptional' "extensionType" transactionExtendedDataExtensionTypeDecoder
    <*> atKeyOptional' "extendedDescription"
          (TransactionExtendedDataExtendedDescription <$> D.text)
    <*> D.atKey "service" transactionExtendedDataServiceDecoder

transactionExtendedDataEncoder :: Encoder' TransactionExtendedData
transactionExtendedDataEncoder = E.mapLikeObj $ \(TransactionExtendedData payer payee et ed serv) ->
  maybeOrAbsentE "payer" E.text payer .
  maybeOrAbsentE "payee" E.text payee .
  maybeOrAbsentE "extensionType" transactionExtendedDataExtensionTypeEncoder et .
  maybeOrAbsentE "extendedDescription" (unTransactionExtendedDataExtendedDescription >$< E.text) ed .
  E.atKey' "service" transactionExtendedDataServiceEncoder serv


-- | Optional extended data provided specific to transaction originated via NPP.
data TransactionExtendedDataExtendedDescription =
  TransactionExtendedDataExtendedDescription { unTransactionExtendedDataExtendedDescription :: Text }
  deriving (Show, Eq)


-- | Optional extended data provided specific to transaction originated via NPP.
data TransactionExtendedDataExtensionType =
  ExtendedDescription
  deriving (Show, Eq)

transactionExtendedDataExtensionTypeText ::
  Prism' Text TransactionExtendedDataExtensionType
transactionExtendedDataExtensionTypeText =
  prism (\case
          ExtendedDescription -> "extendedDescription"
      )
      (\case
          "extendedDescription" -> Right ExtendedDescription
          t -> Left t
      )

transactionExtendedDataExtensionTypeDecoder ::
  Monad f => Decoder f TransactionExtendedDataExtensionType
transactionExtendedDataExtensionTypeDecoder =
  D.prismDOrFail (D.ConversionFailure "Unexpected value in extension$type") transactionExtendedDataExtensionTypeText D.text

transactionExtendedDataExtensionTypeEncoder ::
  Encoder' TransactionExtendedDataExtensionType
transactionExtendedDataExtensionTypeEncoder =
  (transactionExtendedDataExtensionTypeText #) >$< E.text'


data TransactionExtendedDataService =
  X2P101
  deriving (Show, Eq)

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
