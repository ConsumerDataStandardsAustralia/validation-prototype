{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Web.ConsumerData.Au.Api.Types.Common
  ( module Web.ConsumerData.Au.Api.Types.Common
  , module Customer
  ) where

import Control.Lens        (Getter, to)
import GHC.Generics        (Generic)
import Servant.API         ((:>))
import Servant.API.Generic ((:-), AsApi, ToServant, fromServant)
import Servant.Links       (AsLink, Link)

import Web.ConsumerData.Au.Api.Types.Common.Customer as Customer

type CustomerRoute r = r :- "customer" :> ToServant CustomerApi AsApi

data CommonApi r = CommonApi
  { _customer :: CustomerRoute r
  } deriving (Generic)

customer :: Getter (CommonApi r) (CustomerRoute r)
customer = to _customer

customerLinks :: Getter (CommonApi (AsLink Link)) (CustomerApi (AsLink Link))
customerLinks = customer . to fromServant
