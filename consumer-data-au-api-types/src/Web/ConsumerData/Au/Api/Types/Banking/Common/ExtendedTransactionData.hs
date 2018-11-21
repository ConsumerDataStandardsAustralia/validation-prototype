{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.ExtendedTransactionData
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.ExtendedTransactionData
  ) where

import           Control.Lens                        (Prism', prism, ( # ))
import           Data.Functor.Contravariant          ((>$<))
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Waargonaut.Decode                   (Decoder)
import qualified Waargonaut.Decode                   as D
import qualified Waargonaut.Decode.Error             as D
import           Waargonaut.Encode                   (Encoder')
import qualified Waargonaut.Encode                   as E


-- Contains more detailed information specific to transactions originated via NPP. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaextendedtransactiondata CDR AU v0.1.0 ExtendedTransactionData>
data ExtendedTransactionData = ExtendedTransactionData
  { _extendedTransactionDataPayer :: Maybe Text -- ^ Label of the originating payer. Mandatory for an inbound payment.
  , _extendedTransactionDataPayee :: Maybe Text -- ^ Label of the target PayID. Mandatory for an outbound payment.
  , _extendedTransactionDataExtendedDescription :: Maybe ExtendedTransactionDataExtendedDescription -- ^ An extended string description. Only present if specified by the extension$type field.
  , _extendedTransactionDataService :: ExtendedTransactionDataService -- ^ Identifier of the applicable overlay service.
  } deriving (Generic, Show, Eq)

extendedTransactionDataDecoder :: Monad f => Decoder f ExtendedTransactionData
extendedTransactionDataDecoder = D.withCursor $ \c -> do
  o <- D.down c
  payer <- D.fromKey "payer" (D.maybeOrNull D.text) o
  payee <- D.fromKey "payee" (D.maybeOrNull D.text) o
  ed <- D.fromKey "extendedDescription"
    (D.maybeOrNull $ ExtendedTransactionDataExtendedDescription <$> D.text) o
  serv <- D.fromKey "service" extendedTransactionDataServiceDecoder o
  pure $ ExtendedTransactionData payer payee ed serv

extendedTransactionDataEncoder :: Encoder' ExtendedTransactionData
extendedTransactionDataEncoder = E.mapLikeObj $ \(ExtendedTransactionData payer payee ed serv) ->
  E.atKey' "payer" (E.maybeOrNull E.text) payer .
  E.atKey' "payee" (E.maybeOrNull E.text) payee .
  E.atKey' "extendedDescription" (E.maybeOrNull (unExtendedTransactionDataExtendedDescription >$< E.text)) ed .
  E.atKey' "service" extendedTransactionDataServiceEncoder serv

-- | Optional extended data provided specific to transaction originated via NPP.
data ExtendedTransactionDataExtendedDescription =
  ExtendedTransactionDataExtendedDescription { unExtendedTransactionDataExtendedDescription :: Text }
  deriving (Show, Eq)

-- | Optional extended data provided specific to transaction originated via NPP.
data ExtendedTransactionDataExtensionType =
  ExtendedDescription
  deriving (Show, Eq)

extendedTransactionDataExtensionTypeText ::
  Prism' Text ExtendedTransactionDataExtensionType
extendedTransactionDataExtensionTypeText =
  prism (\case
          ExtendedDescription -> "extendedDescription"
      )
      (\case
          "extendedDescription" -> Right ExtendedDescription
          t -> Left t
      )

extendedTransactionDataExtensionTypeEncoder ::
  Encoder' ExtendedTransactionDataExtensionType
extendedTransactionDataExtensionTypeEncoder =
  (extendedTransactionDataExtensionTypeText #) >$< E.text'


data ExtendedTransactionDataService =
  X2P101
  deriving (Show, Eq)

extendedTransactionDataServiceText ::
  Prism' Text ExtendedTransactionDataService
extendedTransactionDataServiceText =
  prism (\case
          X2P101 -> "X2P1.01"
      )
      (\case
          "X2P1.01" -> Right X2P101
          t -> Left t
      )

extendedTransactionDataServiceDecoder ::
  Monad f => Decoder f ExtendedTransactionDataService
extendedTransactionDataServiceDecoder =
  D.prismDOrFail (D.ConversionFailure "ExtendedTransactionDataService")
    extendedTransactionDataServiceText D.text

extendedTransactionDataServiceEncoder ::
  Encoder' ExtendedTransactionDataService
extendedTransactionDataServiceEncoder =
  (extendedTransactionDataServiceText #) >$< E.text'
