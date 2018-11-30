{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.ConsumerData.Au.Api.Types.Banking.Common.Identified where

import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Waargonaut.Decode                   (Decoder)
import qualified Waargonaut.Decode                   as D
import           Waargonaut.Encode                   (Encoder)
import qualified Waargonaut.Encode                   as E

import           Waargonaut.Helpers                  (atKeyOptional', maybeOrAbsentE)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts (AccountId, accountIdDecoder, accountIdEncoder)

data Identified a = Identified
  { _identifiedAccountId :: AccountId
  , _identifiedDisplayName :: Text
  , _identifiedNickName :: Maybe Text
  , _identifiedPayload :: a
  } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

identifiedDecoder :: Monad f => Text -> Decoder f a -> Decoder f (Identified a)
identifiedDecoder key payloadDecoder =
  Identified
    <$> D.atKey "accountId" accountIdDecoder
    <*> D.atKey "displayName" D.text
    <*> atKeyOptional' "nickname" D.text
    <*> D.atKey key payloadDecoder

identifiedEncoder :: Applicative f => Text -> E.Encoder' a -> Encoder f (Identified a)
identifiedEncoder key enc = E.mapLikeObj $ \(Identified accId disp nick p) ->
  E.atKey' "accountId" accountIdEncoder accId .
  E.atKey' "displayName" E.text disp .
  maybeOrAbsentE "nickname" E.text nick .
  E.atKey' key enc p
