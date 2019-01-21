{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Payees where

import Control.Lens

import           Control.Error              (note)
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseQueryParam, parseUrlPiece,
    toQueryParam, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types           (Json, MapLikeObj, WS)

import Waargonaut.Helpers                (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Tag
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (DateString, dateStringDecoder, dateStringEncoder)


newtype PayeeId = PayeeId { unPayeeId :: Text }
  deriving (Eq, Show)

instance ToHttpApiData PayeeId where
  toUrlPiece = toUrlPiece . unPayeeId

instance FromHttpApiData PayeeId where
  parseUrlPiece = fmap PayeeId . parseUrlPiece

payeeIdDecoder :: Monad f => Decoder f PayeeId
payeeIdDecoder = PayeeId <$> D.text

payeeIdEncoder :: Applicative f => Encoder f PayeeId
payeeIdEncoder = unPayeeId >$< E.text


data Payees = Payees { getPayees :: [Payee] }
  deriving (Eq, Show)

instance JsonDecode OB Payees where
  mkDecoder = tagOb payeesDecoder

instance JsonEncode OB Payees where
  mkEncoder = tagOb payeesEncoder

payeesDecoder :: Monad f => Decoder f Payees
payeesDecoder =
  Payees
    <$> D.atKey "payees" (D.list payeeDecoder)

payeesEncoder :: Applicative f => Encoder f Payees
payeesEncoder = E.mapLikeObj $ \(Payees ps) ->
  E.atKey' "payees" (E.list payeeEncoder) ps


data Payee = Payee
  { _payeeId           :: PayeeId
  , _payeeNickname     :: Text
  , _payeeDescription  :: Maybe Text
  , _payeeType         :: PayeeType
  , _payeeCreationDate :: Maybe DateString
  }
  deriving (Eq, Show)

payeeDecoder :: Monad f => Decoder f Payee
payeeDecoder =
  Payee
    <$> D.atKey "payeeId" payeeIdDecoder
    <*> D.atKey "nickname" D.text
    <*> atKeyOptional' "description" D.text
    <*> D.atKey "type" payeeTypeDecoder
    <*> atKeyOptional' "creationDate" dateStringDecoder

payeeEncoder :: Applicative f => Encoder f Payee
payeeEncoder = E.mapLikeObj payeeMLO

payeeMLO :: Payee -> MapLikeObj WS Json -> MapLikeObj WS Json
payeeMLO (Payee pid nick desc ptype cdate) =
  E.atKey' "payeeId" payeeIdEncoder pid .
  E.atKey' "nickname" E.text nick .
  maybeOrAbsentE "description" E.text desc .
  E.atKey' "type" payeeTypeEncoder ptype .
  maybeOrAbsentE "creationDate" dateStringEncoder cdate


data PayeeType =
    Domestic
  | International
  | Biller
  deriving (Bounded, Enum, Eq, Ord, Show)
_PayeeType :: Prism' Text PayeeType
_PayeeType = prism' toT fromT
  where
    toT = \case
      Domestic       -> "DOMESTIC"
      International  -> "INTERNATIONAL"
      Biller         -> "BILLER"
    fromT = \case
      "DOMESTIC"      -> Just Domestic
      "INTERNATIONAL" -> Just International
      "BILLER"        -> Just Biller
      _               -> Nothing

instance ToHttpApiData PayeeType where
  toQueryParam = (_PayeeType #)

instance FromHttpApiData PayeeType where
  parseQueryParam t = note ("Invalid PayeeType: " <> t) (t ^?_PayeeType)

payeeTypeDecoder :: Monad f => Decoder f PayeeType
payeeTypeDecoder = D.prismDOrFail (D.ConversionFailure ("Is not a valid Payee type")) _PayeeType  D.text

payeeTypeEncoder :: Applicative f => Encoder f PayeeType
payeeTypeEncoder = E.prismE _PayeeType E.text

showPayeeType :: PayeeType -> Text
showPayeeType = \case
  Domestic -> "domestic"
  International -> "international"
  Biller -> "biller"
