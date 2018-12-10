{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Discount
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Discount
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


newtype ProductDiscounts =
  ProductDiscounts { getProductDiscounts :: [ProductDiscount] }
  deriving (Eq, Show)

productDiscountsDecoder :: Monad f => Decoder f ProductDiscounts
productDiscountsDecoder = ProductDiscounts <$> D.list productDiscountDecoder

productDiscountsEncoder :: Applicative f => Encoder f ProductDiscounts
productDiscountsEncoder = getProductDiscounts >$< E.list productDiscountEncoder

instance JsonDecode OB ProductDiscounts where
  mkDecoder = tagOb productDiscountsDecoder

instance JsonEncode OB ProductDiscounts where
  mkEncoder = tagOb productDiscountsEncoder


-- | ProductDiscount <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemaproductdiscount CDR AU v0.1.0 ProductDiscount>
data ProductDiscount = ProductDiscount
-- WARNING This type is refering to *Product* only property, not as it defined in the above link
  { _productDiscountDescription  :: Text -- ^ Description of the discount
  , _productDiscountDiscountType :: ProductDiscountType -- ^ The type of discount. See the note below for valid values and their meaning
-- WARNING
  , _productDiscountAmount       :: AmountString -- ^ Value of the discount. Note that the currency of the fee discount is expected to be the same as the currency of the fee itself
  } deriving (Show, Eq)

productDiscountDecoder :: Monad f => Decoder f ProductDiscount
productDiscountDecoder =
  ProductDiscount
    <$> (D.atKey "description" D.text)
    <*> (productDiscountTypeDecoder)
    <*> (D.atKey "amount" amountStringDecoder)

instance JsonDecode OB ProductDiscount where
  mkDecoder = tagOb productDiscountDecoder

productDiscountEncoder :: Applicative f => Encoder f ProductDiscount
productDiscountEncoder = E.mapLikeObj $ \p ->
  E.atKey' "description" E.text (_productDiscountDescription p) .
  productDiscountTypeFields (_productDiscountDiscountType p) .
  E.atKey' "amount" amountStringEncoder (_productDiscountAmount p)

instance JsonEncode OB ProductDiscount where
  mkEncoder = tagOb productDiscountEncoder



-- | Description of the usage of the @discountType@ field as it applies to products. <https://consumerdatastandardsaustralia.github.io/standards/?swagger#productdiscounttypedoc CDR AU v0.1.0 Product Discount Types>
data ProductDiscountType =
    PDiscountBalance AmountString -- ^ "BALANCE" Discount on a fee for maintaining a set balance. As the discount applies to a fee the period is the same as for the fee. Use of @additionalValue@ field: The minimum balance in AmountString format.
  | PDiscountDeposits AmountString -- ^ "DEPOSITS" Discount for depositing a certain amount of money in a period. As the discount applies to a fee the period is the same as for the fee. Use of @additionalValue@ field: The minimum deposit amount in AmountString format.
  | PDiscountPayments AmountString -- ^ "PAYMENTS" Discount for outbound payments from the account under a certain amount of money in a period. As the discount applies to a fee the period is the same as for the fee. Use of @additionalValue@ field: The payment threshold amount in AmountString format.
  | PDiscountBundle Text -- ^ "BUNDLE" Discount for originating a bundle instead of a standalone product. Use of @additionalValue@ field: The name of the applicable bundle.
  deriving (Show, Eq)

productDiscountTypeDecoder :: Monad f => Decoder f ProductDiscountType
productDiscountTypeDecoder = do
  depositRateType <- D.atKey "discountType" D.text
  additionalValue <- case depositRateType of
    "BALANCE" -> PDiscountBalance <$> (additionalValueDecoder amountStringDecoder)
    "DEPOSITS" -> PDiscountDeposits <$> (additionalValueDecoder amountStringDecoder)
    "PAYMENTS" -> PDiscountPayments <$> (additionalValueDecoder amountStringDecoder)
    "BUNDLE" -> PDiscountBundle <$> (additionalValueDecoder D.text)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

productDiscountType'ToText :: ProductDiscountType' -> Text
productDiscountType'ToText = \case
  PDiscountBalance' -> "BALANCE"
  PDiscountDeposits' -> "DEPOSITS"
  PDiscountPayments' -> "PAYMENTS"
  PDiscountBundle' -> "BUNDLE"

data ProductDiscountType' =
    PDiscountBalance'
  | PDiscountDeposits'
  | PDiscountPayments'
  | PDiscountBundle'
  deriving (Eq, Show)

productDiscountType'Encoder :: Applicative f => Encoder f ProductDiscountType'
productDiscountType'Encoder = flip contramap E.text productDiscountType'ToText

productDiscountTypeToType' :: ProductDiscountType -> ProductDiscountType'
productDiscountTypeToType' (PDiscountBalance {})  = PDiscountBalance'
productDiscountTypeToType' (PDiscountDeposits {}) = PDiscountDeposits'
productDiscountTypeToType' (PDiscountPayments {}) = PDiscountPayments'
productDiscountTypeToType' (PDiscountBundle {})   = PDiscountBundle'

productDiscountTypeFields :: (Monoid ws, Semigroup ws) => ProductDiscountType -> MapLikeObj ws Json -> MapLikeObj ws Json
productDiscountTypeFields pc =
  case pc of
    PDiscountBalance v ->
      E.atKey' "discountType" productDiscountType'Encoder (productDiscountTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    PDiscountDeposits v ->
      E.atKey' "discountType" productDiscountType'Encoder (productDiscountTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    PDiscountPayments v ->
      E.atKey' "discountType" productDiscountType'Encoder (productDiscountTypeToType' pc) .
      E.atKey' "additionalValue" amountStringEncoder v
    PDiscountBundle v ->
      E.atKey' "discountType" productDiscountType'Encoder (productDiscountTypeToType' pc) .
      E.atKey' "additionalValue" E.text v

instance JsonDecode OB ProductDiscountType where
  mkDecoder = tagOb productDiscountTypeDecoder
