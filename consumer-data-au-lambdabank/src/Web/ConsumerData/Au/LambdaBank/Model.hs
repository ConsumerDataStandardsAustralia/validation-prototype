{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Web.ConsumerData.Au.LambdaBank.Model where

import Web.ConsumerData.Au.Api.Types

import Control.Monad.Free        (MonadFree, liftF)
import Control.Monad.Free.Church (F, iterM)
import Control.Monad.Free.TH     (makeFreeCon)

import Web.ConsumerData.Au.LambdaBank.FakeData

data ModelF next where
  GetCustomer       :: (CustomerResponse -> next) -> ModelF next
  GetCustomerDetail :: (CustomerDetailResponse -> next) -> ModelF next

deriving instance Functor ModelF

makeFreeCon 'GetCustomer
makeFreeCon 'GetCustomerDetail

type ModelM = F ModelF

runModelM :: Monad m => ModelM a -> m a
runModelM = iterM $ \case
  (GetCustomer next) -> next (CustomerPerson testPerson)
  (GetCustomerDetail next) -> next (CustomerDetailPerson testPersonDetail)
