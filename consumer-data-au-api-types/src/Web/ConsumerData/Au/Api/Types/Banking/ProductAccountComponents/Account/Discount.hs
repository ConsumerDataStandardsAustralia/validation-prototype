{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Discount
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Account.Discount
  ) where

import           Control.Monad.Except       (throwError)
import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import qualified Waargonaut.Decode.Error    as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E
import           Waargonaut.Generic         (JsonDecode (..), JsonEncode (..))
import           Waargonaut.Types.JObject   (MapLikeObj)
import           Waargonaut.Types.Json      (Json)

import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, amountStringDecoder, amountStringEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype AccountDiscounts =
  AccountDiscounts { getAccountDiscounts :: [AccountDiscount] }
  deriving (Eq, Show)

accountDiscountsDecoder :: Monad f => Decoder f AccountDiscounts
accountDiscountsDecoder = AccountDiscounts <$> D.list accountDiscountDecoder

accountDiscountsEncoder :: Applicative f => Encoder f AccountDiscounts
accountDiscountsEncoder = getAccountDiscounts >$< E.list accountDiscountEncoder

instance JsonDecode OB AccountDiscounts where
  mkDecoder = tagOb accountDiscountsDecoder

instance JsonEncode OB AccountDiscounts where
  mkEncoder = tagOb accountDiscountsEncoder



-- | AccountDiscount <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductdiscount CDR AU v0.1.0 ProductDiscount>
-- WARNING This type is refering to *Account* only property, not as it defined in the above link
data AccountDiscount = AccountDiscount
  { _accountDiscountDescription  :: Text -- ^ Description of the discount
  , _accountDiscountDiscountType :: AccountDiscountType -- ^ The type of discount. See the note below for valid values and their meaning
-- WARNING
  , _accountDiscountAmount       :: AmountString -- ^ Value of the discount. Note that the currency of the fee discount is expected to be the same as the currency of the fee itself
  } deriving (Show, Eq)

accountDiscountDecoder :: Monad f => Decoder f AccountDiscount
accountDiscountDecoder =
  AccountDiscount
    <$> (D.atKey "description" D.text)
    <*> (accountDiscountTypeDecoder)
    <*> (D.atKey "amount" amountStringDecoder)

instance JsonDecode OB AccountDiscount where
  mkDecoder = tagOb accountDiscountDecoder

accountDiscountEncoder :: Applicative f => Encoder f AccountDiscount
accountDiscountEncoder = E.mapLikeObj $ \p ->
  E.atKey' "description" E.text (_accountDiscountDescription p) .
  accountDiscountTypeFields (_accountDiscountDiscountType p) .
  E.atKey' "amount" amountStringEncoder (_accountDiscountAmount p)

instance JsonEncode OB AccountDiscount where
  mkEncoder = tagOb accountDiscountEncoder



-- | Description of the usage of the @discountType@ field as it applies to accounts. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#accountdiscounttypedoc CDR AU v0.1.0 Accountt Discount Types>
data AccountDiscountType =
    ADiscountBalance AmountString -- ^ "BALANCE" Discount on a fee for maintaining a set balance. As the discount applies to a fee the period is the same as for the fee. Use of @additionalValue@ field: The minimum balance in AmountString format.
  | ADiscountDeposits AmountString -- ^ "DEPOSITS" Discount for depositing a certain amount of money in a period. As the discount applies to a fee the period is the same as for the fee. Use of @additionalValue@ field: The minimum deposit amount in AmountString format.
  | ADiscountPayments AmountString -- ^ "PAYMENTS" Discount for outbound payments from the account under a certain amount of money in a period. As the discount applies to a fee the period is the same as for the fee. Use of @additionalValue@ field: The payment threshold amount in AmountString format.
  | ADiscountBundle Text -- ^ "BUNDLE" Discount for originating a bundle instead of a standalone product. Use of @additionalValue@ field: The name of the applicable bundle.
  deriving (Show, Eq)

accountDiscountTypeDecoder :: Monad f => Decoder f AccountDiscountType
accountDiscountTypeDecoder = do
  depositRateType <- D.atKey "discountType" D.text
  additionalValue <- case depositRateType of
    "BALANCE" -> ADiscountBalance <$> (additionalValueDecoder amountStringDecoder)
    "DEPOSITS" -> ADiscountDeposits <$> (additionalValueDecoder amountStringDecoder)
    "PAYMENTS" -> ADiscountPayments <$> (additionalValueDecoder amountStringDecoder)
    "BUNDLE" -> ADiscountBundle <$> (additionalValueDecoder D.text)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

accountDiscountType'ToText :: AccountDiscountType' -> Text
accountDiscountType'ToText = \case
  ADiscountBalance' -> "BALANCE"
  ADiscountDeposits' -> "DEPOSITS"
  ADiscountPayments' -> "PAYMENTS"
  ADiscountBundle' -> "BUNDLE"

data AccountDiscountType' =
    ADiscountBalance'
  | ADiscountDeposits'
  | ADiscountPayments'
  | ADiscountBundle'
  deriving (Eq, Show)

accountDiscountType'Encoder :: Applicative f => Encoder f AccountDiscountType'
accountDiscountType'Encoder = flip contramap E.text accountDiscountType'ToText

accountDiscountTypeToType' :: AccountDiscountType -> AccountDiscountType'
accountDiscountTypeToType' (ADiscountBalance {})  = ADiscountBalance'
accountDiscountTypeToType' (ADiscountDeposits {}) = ADiscountDeposits'
accountDiscountTypeToType' (ADiscountPayments {}) = ADiscountPayments'
accountDiscountTypeToType' (ADiscountBundle {})   = ADiscountBundle'

accountDiscountTypeFields :: (Monoid ws, Semigroup ws) => AccountDiscountType -> MapLikeObj ws Json -> MapLikeObj ws Json
accountDiscountTypeFields pc =
  case pc of
    ADiscountBalance v ->
      E.atKey' "discountType" accountDiscountType'Encoder (accountDiscountTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    ADiscountDeposits v ->
      E.atKey' "discountType" accountDiscountType'Encoder (accountDiscountTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    ADiscountPayments v ->
      E.atKey' "discountType" accountDiscountType'Encoder (accountDiscountTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    ADiscountBundle v ->
      E.atKey' "discountType" accountDiscountType'Encoder (accountDiscountTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB AccountDiscountType where
  mkDecoder = tagOb accountDiscountTypeDecoder
