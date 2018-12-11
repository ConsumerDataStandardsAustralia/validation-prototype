{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts where

import           Control.Monad.Except       (throwError)
import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text)
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

import           Waargonaut.Helpers         (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.CurrencyAmount
    (CurrencyAmount, currencyAmountDecoder, currencyAmountEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
import Web.ConsumerData.Au.Api.Types.Tag

data Account = Account
  { _accountAccountId       :: AccountId
  , _accountDisplayName     :: Text
  , _accountNickname        :: Maybe Text
  , _accountMaskedNumber    :: MaskedAccountNumber
  , _accountProductCategory :: Maybe ProductCategory
  , _accountProductType     :: Text
  , _accountBalance         :: Balance
  }
  deriving (Eq, Show)

accountDecoder :: Monad f => Decoder f Account
accountDecoder = do
  balType <- D.atKey "balance$type" balanceTypeDecoder
  Account
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey "maskedNumber" maskedAccountNumberDecoder
    <*> atKeyOptional' "productCategory" productCategoryDecoder
    <*> D.atKey "providerType" D.text
    <*> D.atKey (balanceTypeToText balType) (balanceDecoder balType)

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
  maybeOrAbsentE "productCategory" productCategoryEncoder (_accountProductCategory a) .
  E.atKey' "providerType" E.text (_accountProductType a) .
-- WARNING -^ providerType (in swagger/online) vs productType (in pdf)
  E.atKey' "balance$type" (balanceToType >$< balanceTypeEncode) (_accountBalance a) .
  E.atKey' (balanceTypeToText $ balanceToType (_accountBalance a)) balanceEncoder (_accountBalance a)

instance JsonEncode OB Account where
  mkEncoder = tagOb accountEncoder

newtype Accounts = Accounts { unAccounts :: [Account] } deriving (Eq, Show)

accountsDecoder :: Monad f => Decoder f Accounts
accountsDecoder = D.atKey "accounts" (Accounts <$> D.list accountDecoder)

accountsEncoder :: Applicative f => Encoder f Accounts
accountsEncoder = E.mapLikeObj $ E.atKey' "accounts" (E.list accountEncoder) . unAccounts

instance JsonDecode OB Accounts where
  mkDecoder = tagOb accountsDecoder

instance JsonEncode OB Accounts where
  mkEncoder = tagOb accountsEncoder

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

maskedAccountNumberDecoder :: Monad f => Decoder f MaskedAccountNumber
maskedAccountNumberDecoder = MaskedAccountNumber <$> D.text

maskedAccountNumberEncoder :: Applicative f => Encoder f MaskedAccountNumber
maskedAccountNumberEncoder = unMaskedAccountNumber >$< E.text

data BalanceType
  = BTDeposits
  | BTLending
  | BTPurses
  deriving (Eq, Show)

balanceToType :: Balance -> BalanceType
balanceToType (Deposits {}) = BTDeposits
balanceToType (Lending {})  = BTLending
balanceToType (Purses {})   = BTPurses

balanceTypeDecoder :: Monad f => Decoder f BalanceType
balanceTypeDecoder = D.withCursor $ \c -> D.focus D.text c >>= \case
  "deposits" -> pure BTDeposits
  "lending" -> pure BTLending
  "purses" -> pure BTPurses
  other -> throwError (D.ParseFailed other) -- D.KeyDecodeFailed

balanceTypeEncode :: Applicative f => Encoder f BalanceType
balanceTypeEncode = flip contramap E.text balanceTypeToText

balanceTypeToText :: BalanceType -> Text
balanceTypeToText = \case
  BTDeposits -> "deposits"
  BTLending -> "lending"
  BTPurses -> "purses"

data Balance
  = Deposits
  { _depositBalanceTypeCurrentBalance   :: CurrencyAmount
  , _depositBalanceTypeAvailableBalance :: CurrencyAmount
  }
  | Lending
  { _lendingBalanceTypeCurrentBalance   :: CurrencyAmount
  , _lendingBalanceTypeAvailableBalance :: CurrencyAmount
  , _lendingBalanceCreditLimit          :: CurrencyAmount
  , _lendingBalanceAmortisedLimit       :: Maybe CurrencyAmount
  }
  | Purses
  { _multiCurrencyPursesTypePurses :: [CurrencyAmount]
  }
  deriving (Eq, Show)

balanceDecoder :: Monad f => BalanceType -> Decoder f Balance
balanceDecoder bt =
  case bt of
    BTDeposits ->
      Deposits
        <$> D.atKey "currentBalance" currencyAmountDecoder
        <*> D.atKey "availableBalance" currencyAmountDecoder
    BTLending -> do
      Lending
        <$> D.atKey "currentBalance" currencyAmountDecoder
        <*> D.atKey "availableBalance" currencyAmountDecoder
        <*> D.atKey "creditLimit" currencyAmountDecoder
        <*> atKeyOptional' "amortisedLimit" currencyAmountDecoder
    BTPurses ->
      Purses
        <$> D.atKey "purses" (D.list currencyAmountDecoder)

balanceEncoder :: Applicative f => Encoder f Balance
balanceEncoder = E.mapLikeObj $ \case
  Deposits curBal avBal ->
    E.atKey' "currentBalance" currencyAmountEncoder curBal .
    E.atKey' "availableBalance" currencyAmountEncoder avBal
  Lending curBal avBal credLim amLim ->
    E.atKey' "currentBalance" currencyAmountEncoder curBal .
    E.atKey' "availableBalance" currencyAmountEncoder avBal .
    E.atKey' "creditLimit" currencyAmountEncoder credLim .
    maybeOrAbsentE "amortisedLimit" currencyAmountEncoder amLim
  Purses purses ->
    E.atKey' "purses" (E.list currencyAmountEncoder) purses

newtype AccountBalances =
  AccountBalances { getBalances :: [AccountBalance] }
  deriving (Eq, Show)

accountBalancesDecoder :: Monad f => Decoder f AccountBalances
accountBalancesDecoder = AccountBalances <$> D.list accountBalanceDecoder

accountBalancesEncoder :: Applicative f => Encoder f AccountBalances
accountBalancesEncoder = getBalances >$< E.list accountBalanceEncoder

instance JsonDecode OB AccountBalances where
  mkDecoder = tagOb accountBalancesDecoder

instance JsonEncode OB AccountBalances where
  mkEncoder = tagOb accountBalancesEncoder

data AccountBalance = AccountBalance
  { _accountBalanceAccountId :: AccountId
  , _accountBalanceBalance   :: Balance
  }
  deriving (Eq, Show)

accountBalanceDecoder :: Monad f => Decoder f AccountBalance
accountBalanceDecoder = D.withCursor $ \c -> do
  o <- D.down c
  accId <- D.fromKey "accountId" accountIdDecoder o
  balType <- D.fromKey "balance$type" balanceTypeDecoder o
  balance <- D.fromKey "balance" (balanceDecoder balType) o
  pure $ AccountBalance accId balance

accountBalanceEncoder :: Applicative f => Encoder f AccountBalance
accountBalanceEncoder = E.mapLikeObj $ \case
  AccountBalance accId bal ->
    E.atKey' "accountId" accountIdEncoder accId .
    E.atKey' "balance$type" balanceTypeEncode (balanceToType bal) .
    E.atKey' "balance" balanceEncoder bal
