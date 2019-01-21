{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Constraint
  ( module Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.Product.Constraint
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

import Web.ConsumerData.Au.Api.Types.Banking.ProductAccountComponents.AdditionalValue
    (additionalValueDecoder)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, amountStringDecoder, amountStringEncoder)
import Web.ConsumerData.Au.Api.Types.Tag
    (OB, tagOb)


newtype ProductConstraints =
  ProductConstraints { getProductConstraints :: [ProductConstraint] }
  deriving (Eq, Show)

productConstraintsDecoder :: Monad f => Decoder f ProductConstraints
productConstraintsDecoder = ProductConstraints <$> D.list productConstraintDecoder

productConstraintsEncoder :: Applicative f => Encoder f ProductConstraints
productConstraintsEncoder = getProductConstraints >$< E.list productConstraintEncoder

instance JsonDecode OB ProductConstraints where
  mkDecoder = tagOb productConstraintsDecoder

instance JsonEncode OB ProductConstraints where
  mkEncoder = tagOb productConstraintsEncoder



data ProductConstraint =
    PConstraintMinBalance { unAdditionalValue :: AmountString }
    -- ^ "MIN_BALANCE" A minimum balance is required for the product. Use of @additionalValue@ field: The minimum balance in AmountString format.
  | PConstraintOpeningBalance { unAdditionalValue :: AmountString }
    -- ^ "OPENING_BALANCE" An opening balance is required for the product. Use of @additionalValue@ field: The minimum opening balance in AmountString format.
  | PConstraintMaxLimit { unAdditionalValue :: AmountString }
    -- ^ "MAX_LIMIT" A maximum credit limit exists. Use of @additionalValue@ field: The maximum limit in AmountString format.
  | PConstraintMinLimit { unAdditionalValue :: AmountString }
    -- ^ "MIN_LIMIT" A minimum credit limit exists. Use of @additionalValue@ field: The minimum limit in AmountString format.
  deriving (Eq, Show)

productConstraintDecoder :: Monad f => Decoder f ProductConstraint
productConstraintDecoder = do
  constraintType <- D.atKey "constraintType" D.text
  additionalValue <- case constraintType of
    "MIN_BALANCE" -> PConstraintMinBalance <$> (additionalValueDecoder amountStringDecoder)
    "OPENING_BALANCE" -> PConstraintOpeningBalance <$> (additionalValueDecoder amountStringDecoder)
    "MAX_LIMIT" -> PConstraintMaxLimit <$> (additionalValueDecoder amountStringDecoder)
    "MIN_LIMIT" -> PConstraintMinLimit <$> (additionalValueDecoder amountStringDecoder)
    _ -> throwError D.KeyDecodeFailed
  pure additionalValue

productConstraintTypeToText :: ProductConstraintType -> Text
productConstraintTypeToText = \case
  PConstraintTypeMinBalance -> "MIN_BALANCE"
  PConstraintTypeOpeningBalance -> "OPENING_BALANCE"
  PConstraintTypeMaxLimit -> "MAX_LIMIT"
  PConstraintTypeMinLimit -> "MIN_LIMIT"

data ProductConstraintType =
      PConstraintTypeMinBalance
    | PConstraintTypeOpeningBalance
    | PConstraintTypeMaxLimit
    | PConstraintTypeMinLimit
  deriving (Eq, Show)

productConstraintTypeEncoder :: Applicative f => Encoder f ProductConstraintType
productConstraintTypeEncoder = flip contramap E.text productConstraintTypeToText

productConstraintToType :: ProductConstraint -> ProductConstraintType
productConstraintToType (PConstraintMinBalance {}) = PConstraintTypeMinBalance
productConstraintToType (PConstraintOpeningBalance {}) = PConstraintTypeOpeningBalance
productConstraintToType (PConstraintMaxLimit {}) = PConstraintTypeMaxLimit
productConstraintToType (PConstraintMinLimit {}) = PConstraintTypeMinLimit

productConstraintEncoder :: Applicative f => Encoder f ProductConstraint
productConstraintEncoder = E.mapLikeObj $ \pc -> do
  case pc of
    PConstraintMinBalance amount ->
      E.atKey' "constraintType" productConstraintTypeEncoder (productConstraintToType pc) .
      E.atKey' "additionalValue" amountStringEncoder amount
    PConstraintOpeningBalance amount ->
      E.atKey' "constraintType" productConstraintTypeEncoder (productConstraintToType pc) .
      E.atKey' "additionalValue" amountStringEncoder amount
    PConstraintMaxLimit amount ->
      E.atKey' "constraintType" productConstraintTypeEncoder (productConstraintToType pc) .
      E.atKey' "additionalValue" amountStringEncoder amount
    PConstraintMinLimit amount ->
      E.atKey' "constraintType" productConstraintTypeEncoder (productConstraintToType pc) .
      E.atKey' "additionalValue" amountStringEncoder amount

instance JsonDecode OB ProductConstraint where
  mkDecoder = tagOb productConstraintDecoder
