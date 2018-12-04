{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeOperators   #-}
module Web.ConsumerData.Au.Api.Types.Banking
  ( module Web.ConsumerData.Au.Api.Types.Banking
  , module Accounts
  , module Common
  , module Payees
  , module Products
  ) where

import Control.Lens        (Getter, to)
import GHC.Generics        (Generic)
import Servant.API         ((:>))
import Servant.API.Generic ((:-), AsApi, ToServant, fromServant)
import Servant.Links       (AsLink, Link)

import Web.ConsumerData.Au.Api.Types.Banking.Accounts as Accounts
import Web.ConsumerData.Au.Api.Types.Banking.Common   as Common
import Web.ConsumerData.Au.Api.Types.Banking.Payees   as Payees
import Web.ConsumerData.Au.Api.Types.Banking.Products as Products

type BankingAccountsRoute r = r :- "accounts" :> ToServant AccountsApi AsApi
type BankingPayeesRoute r = r :- "payees" :> ToServant PayeesApi AsApi
type BankingProductsRoute r = r :- "products" :> ToServant ProductsApi AsApi

data BankingApi r = BankingApi
  { _bankingAccounts :: BankingAccountsRoute r
  , _bankingPayees   :: BankingPayeesRoute r
  , _bankingProducts :: BankingProductsRoute r
  } deriving (Generic)

bankingAccounts :: Getter (BankingApi r) (BankingAccountsRoute r)
bankingAccounts = to _bankingAccounts
bankingAccountsLinks :: Getter (BankingApi (AsLink Link)) (AccountsApi (AsLink Link))
bankingAccountsLinks = bankingAccounts . to fromServant

bankingPayees :: Getter (BankingApi r) (BankingPayeesRoute r)
bankingPayees = to _bankingPayees
bankingPayeesLinks :: Getter (BankingApi (AsLink Link)) (PayeesApi (AsLink Link))
bankingPayeesLinks = bankingPayees . to fromServant

bankingProducts :: Getter (BankingApi r) (BankingProductsRoute r)
bankingProducts = to _bankingProducts
bankingProductsLinks :: Getter (BankingApi (AsLink Link)) (ProductsApi (AsLink Link))
bankingProductsLinks = bankingProducts . to fromServant
