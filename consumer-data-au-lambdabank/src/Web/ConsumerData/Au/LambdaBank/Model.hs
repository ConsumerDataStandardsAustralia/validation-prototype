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

import Web.ConsumerData.Au.LambdaBank.FakeData

data ModelF next where
  GetCustomer                               :: (CustomerResponse           -> next) -> ModelF next
  GetCustomerDetail                         :: (CustomerDetailResponse     -> next) -> ModelF next
  GetAccounts                               :: (Accounts  -> next) -> ModelF next
  GetBalancesAll                            :: (AccountBalances -> next) -> ModelF next
  GetBalancesForAccounts                    :: [AccountId] -> (AccountBalances -> next) -> ModelF next
  GetTransactionsAll                        :: (AccountsTransactions -> next) -> ModelF next
  GetTransactionsForAccounts                :: [AccountId] -> (AccountsTransactions -> next) -> ModelF next
  GetDirectDebitsAll                        :: (DirectDebitAuthorisations -> next) -> ModelF next
  GetDirectDebitsForAccounts                :: [AccountId] -> (DirectDebitAuthorisations -> next) -> ModelF next
  GetAccountById                            :: AccountId -> (AccountDetail -> next) -> ModelF next
  GetTransactionsForAccount                 :: AccountId -> (AccountTransactions -> next) -> ModelF next
  GetTransactionDetailForAccountTransaction :: AccountId -> TransactionId -> (AccountTransactionDetail -> next) -> ModelF next
  GetDirectDebitsForAccount                 :: AccountId -> (DirectDebitAuthorisations -> next) -> ModelF next
  GetPayeesAll                              :: (Payees -> next) -> ModelF next
  GetPayeeDetail                            :: PayeeId -> (PayeeDetail -> next) -> ModelF next
  GetProductsAll                            :: (Products -> next) -> ModelF next
  GetProductDetail                          :: ProductId -> (ProductDetail -> next) -> ModelF next

deriving instance Functor ModelF

getCustomer :: MonadFree ModelF m => m CustomerResponse
getCustomer = liftF $ GetCustomer id

getCustomerDetail :: MonadFree ModelF m => m CustomerDetailResponse
getCustomerDetail = liftF $ GetCustomerDetail id

getAccounts :: MonadFree ModelF m => m Accounts
getAccounts = liftF $ GetAccounts id

getBalancesAll :: MonadFree ModelF m => m AccountBalances
getBalancesAll = liftF $ GetBalancesAll id

getBalancesForAccounts :: MonadFree ModelF m => [AccountId] -> m AccountBalances
getBalancesForAccounts aIds = liftF $ GetBalancesForAccounts aIds id

getTransactionsAll :: MonadFree ModelF m => m AccountsTransactions
getTransactionsAll = liftF $ GetTransactionsAll id

getTransactionsForAccounts :: MonadFree ModelF m => [AccountId] -> m AccountsTransactions
getTransactionsForAccounts aIds = liftF $ GetTransactionsForAccounts aIds id

getDirectDebitsAll :: MonadFree ModelF m => m DirectDebitAuthorisations
getDirectDebitsAll = liftF $ GetDirectDebitsAll id

getDirectDebitsForAccounts :: MonadFree ModelF m => [AccountId] -> m DirectDebitAuthorisations
getDirectDebitsForAccounts aIds = liftF $ GetDirectDebitsForAccounts aIds id

getAccountById :: MonadFree ModelF m => AccountId -> m AccountDetail
getAccountById accountId = liftF $ GetAccountById accountId id

getTransactionsForAccount :: MonadFree ModelF m => AccountId -> m AccountTransactions
getTransactionsForAccount accountId = liftF $ GetTransactionsForAccount accountId id

getTransactionDetailForAccountTransaction :: MonadFree ModelF m => AccountId -> TransactionId -> m AccountTransactionDetail
getTransactionDetailForAccountTransaction aId xactId = liftF $ GetTransactionDetailForAccountTransaction aId xactId id

getDirectDebitsForAccount :: MonadFree ModelF m => AccountId -> m DirectDebitAuthorisations
getDirectDebitsForAccount accountId = liftF $ GetDirectDebitsForAccount accountId id

getPayeesAll :: MonadFree ModelF m => m Payees
getPayeesAll = liftF $ GetPayeesAll id

getPayeeDetail :: MonadFree ModelF m => PayeeId -> m PayeeDetail
getPayeeDetail pId = liftF $ GetPayeeDetail pId id

getProductsAll :: MonadFree ModelF m => m Products
getProductsAll = liftF $ GetProductsAll id

getProductDetail :: MonadFree ModelF m => ProductId -> m ProductDetail
getProductDetail pId = liftF $ GetProductDetail pId id

type ModelM = F ModelF

-- This will have to take some kind of config later and the calls should actually
-- take proper inputs (like the user id for get customer). But this works well
-- enough for now.
runModelM :: Monad m => ModelM a -> m a
runModelM = iterM $ \case
  (GetCustomer next) -> next (CustomerPerson testPerson)
  (GetCustomerDetail next) -> next (CustomerDetailPerson testPersonDetail)
  (GetAccounts next) -> next testAccounts
  (GetBalancesAll next) -> next testBalances
  (GetBalancesForAccounts _ next) -> next testBalances
  (GetTransactionsAll next) -> next testAccountsTransactions
  (GetTransactionsForAccounts _ next) -> next testAccountsTransactions
  (GetDirectDebitsAll next) -> next testDirectDebitAuthorisations
  (GetDirectDebitsForAccounts _ next) -> next testDirectDebitAuthorisations
  (GetAccountById _ next) -> next testAccountDetail
  (GetTransactionsForAccount _ next) -> next testAccountTransactions
  (GetTransactionDetailForAccountTransaction _ _ next) -> next testAccountTransactionDetail
  (GetDirectDebitsForAccount _ next) -> next testDirectDebitAuthorisations
  (GetPayeesAll next) -> next testPayees
  (GetPayeeDetail _ next) -> next testPayeeDetail
  (GetProductsAll next) -> next testProducts
  (GetProductDetail _ next) -> next testProductDetail
