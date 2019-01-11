{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts where

import           Control.Lens
import           Data.Bool                  (bool)
import           Data.Char                  (isNumber)
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
import Web.ConsumerData.Au.Api.Types.Tag


newtype Accounts = Accounts { unAccounts :: [Account] } deriving (Eq, Show)

accountsDecoder :: Monad f => Decoder f Accounts
accountsDecoder = D.atKey "accounts" (Accounts <$> D.list accountDecoder)

accountsEncoder :: Applicative f => Encoder f Accounts
accountsEncoder = E.mapLikeObj $ E.atKey' "accounts" (E.list accountEncoder) . unAccounts

instance JsonDecode OB Accounts where
  mkDecoder = tagOb accountsDecoder

instance JsonEncode OB Accounts where
  mkEncoder = tagOb accountsEncoder


data Account = Account
  { _accountAccountId       :: AccountId
  , _accountDisplayName     :: Text
  , _accountNickname        :: Maybe Text
  , _accountMaskedNumber    :: MaskedAccountNumber
  , _accountOpenStatus      :: Maybe AccOpenStatus
  , _accountIsOwned         :: Maybe Bool
  , _accountProductCategory :: EnumProductCategory
  , _accountProductName     :: Text
  }
  deriving (Eq, Show)

accountDecoder :: Monad f => Decoder f Account
accountDecoder =
  Account
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey "maskedNumber" maskedAccountNumberDecoder
    <*> atKeyOptional' "openStatus" accOpenStatusDecoder
    <*> atKeyOptional' "isOwned" D.bool
    <*> D.atKey "productCategory" enumProductCategoryDecoder
    <*> D.atKey "productName" D.text

instance JsonDecode OB Account where
  mkDecoder = tagOb accountDecoder

accountEncoder :: Applicative f => Encoder f Account
accountEncoder =
  E.mapLikeObj accountFields

accountFields :: (Monoid ws, Semigroup ws) => Account -> MapLikeObj ws Json -> MapLikeObj ws Json
accountFields a =
  E.atKey' "accountId" accountIdEncoder (_accountAccountId a) .
  E.atKey' "displayName" E.text (_accountDisplayName a) .
  maybeOrAbsentE "nickname" E.text (_accountNickname a) .
  E.atKey' "maskedNumber" maskedAccountNumberEncoder (_accountMaskedNumber a) .
  maybeOrAbsentE "openStatus" accOpenStatusEncoder (_accountOpenStatus a) .
  maybeOrAbsentE "isOwned" E.bool (_accountIsOwned a) .
  E.atKey' "productCategory" enumProductCategoryEncoder (_accountProductCategory a) .
  E.atKey' "productName" E.text (_accountProductName a)

instance JsonEncode OB Account where
  mkEncoder = tagOb accountEncoder


newtype AccountIds = AccountIds { unAccountIds :: [AccountId] } deriving (Eq, Show)

accountIdsDecoder :: Monad f => Decoder f AccountIds
accountIdsDecoder = D.atKey "accountIds" (AccountIds <$> D.list accountIdDecoder)

accountIdsEncoder :: Applicative f => Encoder f AccountIds
accountIdsEncoder = E.mapLikeObj $ E.atKey' "accountIds" (E.list accountIdEncoder) . unAccountIds

instance JsonDecode OB AccountIds where
  mkDecoder = tagOb accountIdsDecoder

instance JsonEncode OB AccountIds where
  mkEncoder = tagOb accountIdsEncoder


newtype AccountId = AccountId { unAccountId :: AsciiString } deriving (Eq, Show)

accountIdDecoder :: Monad f => Decoder f AccountId
accountIdDecoder = AccountId <$> asciiStringDecoder

accountIdEncoder :: Applicative f => Encoder f AccountId
accountIdEncoder = unAccountId >$< asciiStringEncoder

instance ToHttpApiData AccountId where
  toUrlPiece = toUrlPiece . unAccountId
instance FromHttpApiData AccountId where
  parseUrlPiece = fmap AccountId . parseUrlPiece


newtype MaskedAccountNumber =
  MaskedAccountNumber { unMaskedAccountNumber :: Text }
  deriving (Eq, Show)

-- TODO: This isn't quite good enough. The spec says that it is only numbers
-- that get masked. RIP.
-- e.g: 62 1234-5678 => XX XXXX-5678
maskAccountId :: AccountId -> MaskedAccountNumber
maskAccountId (AccountId (AsciiString t)) = MaskedAccountNumber $
  (T.map mask $ T.take nonMasked t) <> (T.takeEnd 4 t)
  where
    chars = T.length t
    mask c = bool c 'X' (isNumber c)
    nonMasked = chars - 4

maskedAccountNumberDecoder :: Monad f => Decoder f MaskedAccountNumber
maskedAccountNumberDecoder = MaskedAccountNumber <$> D.text

maskedAccountNumberEncoder :: Applicative f => Encoder f MaskedAccountNumber
maskedAccountNumberEncoder = unMaskedAccountNumber >$< E.text


data AccOpenStatus = AccOpen | AccClosed
  deriving (Bounded, Enum, Eq, Show)
_AccOpenStatus :: Prism' Text AccOpenStatus
_AccOpenStatus = prism' toT fromT
  where
    toT = \case
      AccOpen   -> "OPEN"
      AccClosed -> "CLOSED"
    fromT = \case
      "OPEN"   -> Just AccOpen
      "CLOSED" -> Just AccClosed
      _        -> Nothing

accOpenStatusEncoder :: E.Encoder' AccOpenStatus
accOpenStatusEncoder =
  E.prismE _AccOpenStatus E.text'

accOpenStatusDecoder :: Monad m =>
  D.Decoder m AccOpenStatus
accOpenStatusDecoder = D.prismDOrFail
  (D._ConversionFailure # "Not a valid AccountOpen Status type")
  _AccOpenStatus
  D.text
