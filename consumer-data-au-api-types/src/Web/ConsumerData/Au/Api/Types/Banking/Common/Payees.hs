{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Payees where

import Control.Monad.Except (throwError)
import Data.Functor.Contravariant (contramap, (>$<))
import Data.Text           (Text)
import qualified Data.Text as T
import Servant.API
    (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import qualified Waargonaut.Decode.Error as D
import           Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E
import           Waargonaut.Generic (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types (MapLikeObj, WS, Json)

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Tag

newtype PayeeId = PayeeId { unPayeeId :: Text } deriving (Eq, Show)
instance ToHttpApiData PayeeId where
  toUrlPiece = toUrlPiece . unPayeeId
instance FromHttpApiData PayeeId where
  parseUrlPiece = fmap PayeeId . parseUrlPiece

data Payee = Payee
  { _payeeId :: PayeeId
  , _payeeNickname :: Text
  , _payeeDescription :: Maybe Text
  , _payeeType :: PayeeType
  }
  deriving (Eq, Show)

data PayeeType = Domestic | International | Biller deriving (Eq, Show)

data Payees = Payees { getPayees :: [Payee] } deriving (Eq, Show)

instance JsonDecode OB Payees where
  mkDecoder = tagOb payeesDecoder

instance JsonEncode OB Payees where
  mkEncoder = tagOb payeesEncoder

payeesDecoder :: Monad f => Decoder f Payees
payeesDecoder = D.withCursor $ \c -> do
  o <- D.down c
  ps <- D.fromKey "payees" (D.list payeeDecoder) o
  pure $ Payees ps

payeesEncoder :: Applicative f => Encoder f Payees
payeesEncoder = E.mapLikeObj $ \(Payees ps) ->
  E.atKey' "payees" (E.list payeeEncoder) ps

payeeDecoder :: Monad f => Decoder f Payee
payeeDecoder =
  Payee
    <$> D.atKey "payeeId" payeeIdDecoder
    <*> D.atKey "nickname" D.text
    <*> atKeyOptional' "description" D.text
    <*> D.atKey "type" payeeTypeDecoder

payeeEncoder :: Applicative f => Encoder f Payee
payeeEncoder = E.mapLikeObj payeeMLO

payeeMLO :: Payee -> MapLikeObj WS Json -> MapLikeObj WS Json
payeeMLO (Payee pid nick desc ptype) =
  E.atKey' "payeeId" payeeIdEncoder pid .
  E.atKey' "nickname" E.text nick .
  maybeOrAbsentE "description" E.text desc .
  E.atKey' "type" (payeeTypeEncoder True) ptype

payeeIdDecoder :: Monad f => Decoder f PayeeId
payeeIdDecoder = PayeeId <$> D.text

payeeIdEncoder :: Applicative f => Encoder f PayeeId
payeeIdEncoder = unPayeeId >$< E.text

payeeTypeDecoder :: Monad f => Decoder f PayeeType
payeeTypeDecoder = D.text >>= \case
  "DOMESTIC" -> pure Domestic
  "domestic" -> pure Domestic
  "INTERNATIONAL" -> pure International
  "international" -> pure International
  "BILLER" -> pure Biller
  "biller" -> pure Biller
  s -> throwError (D.ConversionFailure (s <> " is not a valid Payee type"))

payeeTypeEncoder :: Applicative f => Bool -> Encoder f PayeeType
payeeTypeEncoder upper = flip contramap E.text $ \x ->
  (if upper then T.toUpper else id) $ showPayeeType x

showPayeeType :: PayeeType -> Text
showPayeeType = \case
  Domestic -> "domestic"
  International -> "international"
  Biller -> "biller"
