{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Web.ConsumerData.Au.Api.Types.Banking.Common.CurrencyAmount
  ( module Web.ConsumerData.Au.Api.Types.Banking.Common.CurrencyAmount
  ) where

import           Waargonaut.Decode (Decoder)
import qualified Waargonaut.Decode as D
import           Waargonaut.Encode (Encoder)
import qualified Waargonaut.Encode as E

import Waargonaut.Helpers
    (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, CurrencyString, amountStringDecoder, amountStringEncoder,
    currencyStringDecoder, currencyStringEncoder)


data CurrencyAmount = CurrencyAmount
  { _currencyAmountAmount   :: AmountString
  , _currencyAmountCurrency :: Maybe CurrencyString
  } deriving (Eq, Show)

currencyAmountDecoder :: Monad f => Decoder f CurrencyAmount
currencyAmountDecoder =
  CurrencyAmount
    <$> D.atKey "amount" amountStringDecoder
    <*> atKeyOptional' "currency" currencyStringDecoder

currencyAmountEncoder :: Applicative f => Encoder f CurrencyAmount
currencyAmountEncoder = E.mapLikeObj $ \ca ->
  E.atKey' "amount" amountStringEncoder (_currencyAmountAmount ca) .
  maybeOrAbsentE "currency" currencyStringEncoder (_currencyAmountCurrency ca)
