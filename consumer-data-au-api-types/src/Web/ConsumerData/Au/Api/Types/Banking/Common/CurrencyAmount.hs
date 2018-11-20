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

-- import           Control.Lens               (Prism', prism, ( # ))
--import           Control.Monad.Except       (throwError)
--import           Data.Functor.Contravariant (contramap, (>$<))
import           Data.Text                  (Text)
--import           Servant.API
--    (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import           Waargonaut.Decode          (Decoder)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
    (AmountString, amountStringDecoder, amountStringEncoder)


-- | CurrencyAmount <https://consumerdatastandardsaustralia.github.io/standards/?swagger#schemacurrencyamount CDR AU v0.1.0 CurrencyAmount>
data CurrencyAmount = CurrencyAmount
  { _currencyAmountAmount   :: AmountString
  , _currencyAmountCurrency :: Maybe Text
  } deriving (Eq, Show)

currencyAmountDecoder :: Monad f => Decoder f CurrencyAmount
currencyAmountDecoder = D.withCursor $ \c -> do
  o <- D.down c
  amount <- D.fromKey "amount" amountStringDecoder o
  currency <- D.try $ D.fromKey "currency" D.text o
  pure $ CurrencyAmount amount currency

currencyAmountEncoder :: Applicative f => Encoder f CurrencyAmount
currencyAmountEncoder = E.mapLikeObj $ \ca ->
  E.atKey' "amount" amountStringEncoder (_currencyAmountAmount ca) .
  E.atKey' "currency" (E.maybeOrNull E.text) (_currencyAmountCurrency ca)
