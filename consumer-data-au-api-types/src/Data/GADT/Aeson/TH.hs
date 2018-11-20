{-# LANGUAGE TemplateHaskell #-}

module Data.GADT.Aeson.TH where

import           Data.Aeson.Types     (FromJSON1, ToJSON1 (..), parseJSON1,
                                       toJSON1)
import           Data.GADT.Aeson      (FromJSONViaKey (..),
                                        ToJSONViaKey (..))

import           Language.Haskell.TH

import Data.GADT.Tag.TH (deriveClassForGADT)

deriveFromJSONViaKey, deriveToJSONViaKey ::
  Name
  -> DecsQ

deriveFromJSONViaKey n =
  deriveClassForGADT ''FromJSONViaKey ''FromJSON1 n 'parseJSONViaKey 'parseJSON1

deriveToJSONViaKey n =
  deriveClassForGADT ''ToJSONViaKey ''ToJSON1 n 'toJSONViaKey 'toJSON1
