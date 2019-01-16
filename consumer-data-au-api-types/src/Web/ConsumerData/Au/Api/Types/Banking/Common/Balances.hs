{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Balances where

import           Data.Functor.Contravariant ((>$<))
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
    (AccountId, accountIdDecoder, accountIdEncoder)
import Web.ConsumerData.Au.Api.Types.Banking.Common.CurrencyAmount
    (CurrencyAmount, currencyAmountDecoder, currencyAmountEncoder)
import Web.ConsumerData.Au.Api.Types.SumTypeHelpers
import Web.ConsumerData.Au.Api.Types.Tag (OB, tagOb)


newtype AccountBalances =
  AccountBalances { getBalances :: [Balance] }
  deriving (Eq, Show)

accountBalancesDecoder :: Monad f => Decoder f AccountBalances
accountBalancesDecoder =
  AccountBalances
   <$> D.atKey "balances" (D.list balanceDecoder)

accountBalancesEncoder :: Applicative f => Encoder f AccountBalances
accountBalancesEncoder = E.mapLikeObj $ \(AccountBalances ps) ->
  E.atKey' "balances" (E.list balanceEncoder) ps

instance JsonDecode OB AccountBalances where
  mkDecoder = tagOb accountBalancesDecoder

instance JsonEncode OB AccountBalances where
  mkEncoder = tagOb accountBalancesEncoder


data Balance = Balance
  { _balanceAccountId   :: AccountId
  , _balanceBalanceType :: BalanceType
  }
  deriving (Eq, Show)

balanceDecoder :: Monad f => Decoder f Balance
balanceDecoder =
  Balance
    <$> D.atKey "accountId" accountIdDecoder
    <*> balanceTypeDecoder

balanceEncoder :: Applicative f => Encoder f Balance
balanceEncoder = E.mapLikeObj $ \b ->
    E.atKey' "accountId" accountIdEncoder (_balanceAccountId b) .
    balanceTypeFields (_balanceBalanceType b)


balanceTypeDecoder :: Monad f => Decoder f BalanceType
balanceTypeDecoder = typeTaggedDecoder "balanceUType" $ \case
  "deposit" -> Just $ (TypedTagField BalanceDeposit depositBalanceTypeDecoder)
  "lending"  -> Just $ (TypedTagField BalanceLending lendingBalanceTypeDecoder)
  "purses"   -> Just $ (TypedTagField BalancePurses multiCurrencyPursesTypeDecoder)
  _          -> Nothing

balanceTypeFields ::
  (Monoid ws, Semigroup ws)
  => BalanceType -> MapLikeObj ws Json -> MapLikeObj ws Json
balanceTypeFields = \case
  BalanceDeposit b -> fields "deposit" depositBalanceTypeEncoder b
  BalanceLending b -> fields "lending" lendingBalanceTypeEncoder b
  BalancePurses b  -> fields "purses" multiCurrencyPursesTypeEncoder b
  where
    fields = typeTaggedField "balanceUType"


data BalanceType =
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
  { _lbtAccountBalance   :: CurrencyAmount
  , _lbtAvailableBalance :: CurrencyAmount
  , _lbtCreditLimit      :: CurrencyAmount
  , _lbtAmortisedLimit   :: Maybe CurrencyAmount
  }  deriving (Eq, Show)

lendingBalanceTypeDecoder :: Monad f => Decoder f LendingBalanceType
lendingBalanceTypeDecoder =
  LendingBalanceType
    <$> D.atKey "accountBalance" currencyAmountDecoder
    <*> D.atKey "availableBalance" currencyAmountDecoder
    <*> D.atKey "creditLimit" currencyAmountDecoder
    <*> atKeyOptional' "amortisedLimit" currencyAmountDecoder

lendingBalanceTypeEncoder :: Applicative f => Encoder f LendingBalanceType
lendingBalanceTypeEncoder = E.mapLikeObj $ \b ->
  E.atKey' "accountBalance" currencyAmountEncoder (_lbtAccountBalance b) .
  E.atKey' "availableBalance" currencyAmountEncoder (_lbtAvailableBalance b) .
  E.atKey' "creditLimit" currencyAmountEncoder (_lbtCreditLimit b) .
  maybeOrAbsentE "amortisedLimit" currencyAmountEncoder (_lbtAmortisedLimit b)


data MultiCurrencyPursesType = MultiCurrencyPursesType
  { _mcptPurses :: [CurrencyAmount]
  }  deriving (Eq, Show)

multiCurrencyPursesTypeDecoder :: Monad f => Decoder f MultiCurrencyPursesType
multiCurrencyPursesTypeDecoder =
  MultiCurrencyPursesType
    <$> D.list currencyAmountDecoder

multiCurrencyPursesTypeEncoder :: Applicative f => Encoder f MultiCurrencyPursesType
multiCurrencyPursesTypeEncoder = _mcptPurses >$< E.list currencyAmountEncoder
