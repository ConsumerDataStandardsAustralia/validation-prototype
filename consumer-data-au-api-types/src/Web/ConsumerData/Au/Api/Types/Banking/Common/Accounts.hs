{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts where

-- import           Control.Monad.Except       (throwError)
import           Data.Functor.Contravariant ((>$<)) --contramap
import           Data.Text                  (Text)
import           Servant.API
    (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
-- import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.CurrencyAmount
    (CurrencyAmount, currencyAmountDecoder, currencyAmountEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers
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
  , _accountProductCategory :: Maybe ProductCategory
  , _accountProductType     :: Text
  , _accountBalance         :: Balance
  }
  deriving (Eq, Show)

accountDecoder :: Monad f => Decoder f Account
accountDecoder =
  Account
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey "maskedNumber" maskedAccountNumberDecoder
    <*> atKeyOptional' "productCategory" productCategoryDecoder
    <*> D.atKey "providerType" D.text
    <*> balanceTypeDecoder
{-
    where
      balanceTypeDecoder = typeTaggedDecoder "balance$type" $ \case
        "deposits" -> Just $ (TypedTagField BalanceDeposit depositBalanceTypeDecoder)
        "lending"  -> Just $ (TypedTagField BalanceLending lendingBalanceTypeDecoder)
        "purses"   -> Just $ (TypedTagField BalancePurses multiCurrencyPursesTypeDecoder)
        _          -> Nothing
-}

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
  balanceTypeFields (_accountBalance a)
{-
  where
    balanceTypeFields = \case
      BalanceDeposit b -> fields "deposits" depositBalanceTypeEncoder b
      BalanceLending b -> fields "lending" lendingBalanceTypeEncoder b
      BalancePurses b  -> fields "purses" multiCurrencyPursesTypeEncoder b
    fields = typeTaggedField "balance$type"
-}

instance JsonEncode OB Account where
  mkEncoder = tagOb accountEncoder


balanceTypeDecoder :: Monad f => Decoder f Balance
balanceTypeDecoder = typeTaggedDecoder "balance$type" $ \case
  "deposits" -> Just $ (TypedTagField BalanceDeposit depositBalanceTypeDecoder)
  "lending"  -> Just $ (TypedTagField BalanceLending lendingBalanceTypeDecoder)
  "purses"   -> Just $ (TypedTagField BalancePurses multiCurrencyPursesTypeDecoder)
  _          -> Nothing

balanceTypeFields ::
  (Monoid ws, Semigroup ws)
  => Balance -> MapLikeObj ws Json -> MapLikeObj ws Json
balanceTypeFields = \case
  BalanceDeposit b -> fields "deposits" depositBalanceTypeEncoder b
  BalanceLending b -> fields "lending" lendingBalanceTypeEncoder b
  BalancePurses b  -> fields "purses" multiCurrencyPursesTypeEncoder b
  where
    fields = typeTaggedField "balance$type"


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


-- | The type of the balance object
data Balance =
    BalanceDeposit DepositBalanceType
  | BalanceLending LendingBalanceType
  | BalancePurses MultiCurrencyPursesType
  deriving (Eq, Show)



data DepositBalanceType = DepositBalanceType
  { _dbtCurrentBalance   :: CurrencyAmount
  , _dbtAvailableBalance :: CurrencyAmount
  }  deriving (Eq, Show)

depositBalanceTypeDecoder :: Monad f => Decoder f DepositBalanceType
depositBalanceTypeDecoder =
  DepositBalanceType
    <$> D.atKey "currentBalance" currencyAmountDecoder
    <*> D.atKey "availableBalance" currencyAmountDecoder


depositBalanceTypeEncoder :: Applicative f => Encoder f DepositBalanceType
depositBalanceTypeEncoder = E.mapLikeObj $ \b ->
  E.atKey' "currentBalance" currencyAmountEncoder (_dbtCurrentBalance b) .
  E.atKey' "availableBalance" currencyAmountEncoder (_dbtAvailableBalance b)


data LendingBalanceType = LendingBalanceType
  { _lbtCurrentBalance   :: CurrencyAmount
  , _lbtAvailableBalance :: CurrencyAmount
  , _lbtCreditLimit      :: CurrencyAmount
  , _lbtAmortisedLimit   :: Maybe CurrencyAmount
  }  deriving (Eq, Show)

lendingBalanceTypeDecoder :: Monad f => Decoder f LendingBalanceType
lendingBalanceTypeDecoder =
  LendingBalanceType
    <$> D.atKey "currentBalance" currencyAmountDecoder
    <*> D.atKey "availableBalance" currencyAmountDecoder
    <*> D.atKey "creditLimit" currencyAmountDecoder
    <*> atKeyOptional' "amortisedLimit" currencyAmountDecoder

lendingBalanceTypeEncoder :: Applicative f => Encoder f LendingBalanceType
lendingBalanceTypeEncoder = E.mapLikeObj $ \b ->
  E.atKey' "currentBalance" currencyAmountEncoder (_lbtCurrentBalance b) .
  E.atKey' "availableBalance" currencyAmountEncoder (_lbtAvailableBalance b) .
  E.atKey' "creditLimit" currencyAmountEncoder (_lbtCreditLimit b) .
  maybeOrAbsentE "amortisedLimit" currencyAmountEncoder (_lbtAmortisedLimit b)


data MultiCurrencyPursesType = MultiCurrencyPursesType
  { _mcptPurses :: [CurrencyAmount]
  }  deriving (Eq, Show)

multiCurrencyPursesTypeDecoder :: Monad f => Decoder f MultiCurrencyPursesType
multiCurrencyPursesTypeDecoder =
  MultiCurrencyPursesType
    <$> D.atKey "purses" (D.list currencyAmountDecoder)

multiCurrencyPursesTypeEncoder :: Applicative f => Encoder f MultiCurrencyPursesType
multiCurrencyPursesTypeEncoder = E.mapLikeObj $ \(MultiCurrencyPursesType purses) ->
  E.atKey' "purses" (E.list currencyAmountEncoder) purses



newtype AccountBalances =
  AccountBalances { getBalances :: [AccountBalance] }
  deriving (Eq, Show)

accountBalancesDecoder :: Monad f => Decoder f AccountBalances
accountBalancesDecoder =
  AccountBalances
   <$> D.atKey "balances" (D.list accountBalanceDecoder)

accountBalancesEncoder :: Applicative f => Encoder f AccountBalances
accountBalancesEncoder = E.mapLikeObj $ \(AccountBalances ps) ->
  E.atKey' "balances" (E.list accountBalanceEncoder) ps

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
accountBalanceDecoder =
  AccountBalance
    <$> D.atKey "accountId" accountIdDecoder
    <*> balanceTypeDecoder

accountBalanceEncoder :: Applicative f => Encoder f AccountBalance
accountBalanceEncoder = E.mapLikeObj $ \ab ->
    E.atKey' "accountId" accountIdEncoder (_accountBalanceAccountId ab) .
    balanceTypeFields (_accountBalanceBalance ab)
