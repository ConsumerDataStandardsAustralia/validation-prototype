{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Web.ConsumerData.Au.LambdaBank.Model where

import Web.ConsumerData.Au.Api.Types

import Data.Text                 (Text)
import Control.Monad.Free        (MonadFree, liftF)

import Data.Functor.Coproduct ((:<:), inj)
import Web.ConsumerData.Au.LambdaBank.FakeData

data ModelF next where
  GetCustomer                               :: (CustomerResponse           -> next) -> ModelF next
  GetCustomerDetail                         :: (CustomerDetailResponse     -> next) -> ModelF next
  GetAccounts                               :: (Accounts  -> next) -> ModelF next
  GetBalancesAll                            :: (AccountBalances -> next) -> ModelF next
  GetBalancesForAccounts                    :: AccountIds -> (AccountBalances -> next) -> ModelF next
  GetTransactionsAll                        :: (BulkTransactions -> next) -> ModelF next
  GetTransactionsForAccounts                :: AccountIds -> (BulkTransactions -> next) -> ModelF next
  GetDirectDebitsAll                        :: (DirectDebitAuthorisations -> next) -> ModelF next
  GetDirectDebitsForAccounts                :: AccountIds -> (DirectDebitAuthorisations -> next) -> ModelF next
  GetAccountById                            :: AccountId -> (AccountDetail -> next) -> ModelF next
  GetTransactionsForAccount                 :: AccountId -> (AccountTransactions -> next) -> ModelF next
  GetTransactionDetailForAccountTransaction :: AccountId -> TransactionId -> (TransactionsDetail -> next) -> ModelF next
  GetDirectDebitsForAccount                 :: AccountId -> (DirectDebitAuthorisations -> next) -> ModelF next
  GetPayeesAll
    :: Maybe PayeeType
    -> Maybe PageNumber
    -> Maybe PageSize
    -> (Payees -> next)
    -> ModelF next
  GetPayeeDetail                            :: PayeeId -> (PayeeDetail -> next) -> ModelF next
  GetProductsAll
    :: Maybe ProductEffective
    -> Maybe DateTimeString
    -> Maybe Text
    -> Maybe ProductCategory
    -> Maybe PageNumber
    -> Maybe PageSize
    -> (Products -> next) -> ModelF next
  GetProductDetail                          :: ProductId -> (ProductDetail -> next) -> ModelF next

deriving instance Functor ModelF

type ModelFree a = forall f m. (ModelF :<: f, MonadFree f m) => m a

getCustomer :: ModelFree CustomerResponse
getCustomer = liftF . inj $ GetCustomer id

getCustomerDetail :: ModelFree CustomerDetailResponse
getCustomerDetail = liftF . inj $ GetCustomerDetail id

getAccounts :: ModelFree Accounts
getAccounts = liftF . inj $ GetAccounts id

getBalancesAll :: ModelFree AccountBalances
getBalancesAll = liftF . inj $ GetBalancesAll id

getBalancesForAccounts :: AccountIds -> ModelFree AccountBalances
getBalancesForAccounts aIds = liftF . inj $ GetBalancesForAccounts aIds id

getTransactionsAll :: ModelFree BulkTransactions
getTransactionsAll = liftF . inj $ GetTransactionsAll id

getTransactionsForAccounts :: AccountIds -> ModelFree BulkTransactions
getTransactionsForAccounts aIds = liftF . inj $ GetTransactionsForAccounts aIds id

getDirectDebitsAll :: ModelFree DirectDebitAuthorisations
getDirectDebitsAll = liftF . inj $ GetDirectDebitsAll id

getDirectDebitsForAccounts :: AccountIds -> ModelFree DirectDebitAuthorisations
getDirectDebitsForAccounts aIds = liftF . inj $ GetDirectDebitsForAccounts aIds id

getAccountById :: AccountId -> ModelFree AccountDetail
getAccountById accountId = liftF . inj $ GetAccountById accountId id

getTransactionsForAccount :: AccountId -> ModelFree AccountTransactions
getTransactionsForAccount accountId = liftF . inj $ GetTransactionsForAccount accountId id

getTransactionDetailForAccountTransaction :: AccountId -> TransactionId -> ModelFree TransactionsDetail
getTransactionDetailForAccountTransaction aId xactId = liftF . inj $ GetTransactionDetailForAccountTransaction aId xactId id

getDirectDebitsForAccount :: AccountId -> ModelFree DirectDebitAuthorisations
getDirectDebitsForAccount accountId = liftF . inj $ GetDirectDebitsForAccount accountId id

getPayeesAll :: Maybe PayeeType -> Maybe PageNumber -> Maybe PageSize -> ModelFree Payees
getPayeesAll pt pn ps = liftF . inj $ GetPayeesAll pt pn ps id

getPayeeDetail :: PayeeId -> ModelFree PayeeDetail
getPayeeDetail pId = liftF . inj $ GetPayeeDetail pId id

getProductsAll
  :: Maybe ProductEffective
  -> Maybe DateTimeString
  -> Maybe Text
  -> Maybe ProductCategory
  -> Maybe PageNumber
  -> Maybe PageSize
  -> ModelFree Products
getProductsAll pe dts t pc pn ps = liftF . inj $ GetProductsAll pe dts t pc pn ps id

getProductDetail :: ProductId -> ModelFree ProductDetail
getProductDetail pId = liftF . inj $ GetProductDetail pId id


filterBalancesByAccountIds :: AccountIds -> AccountBalances -> AccountBalances
filterBalancesByAccountIds (AccountIds aIds) (AccountBalances balances) =
  AccountBalances $ filter (\balance -> elem (_accountBalanceAccountId balance) aIds) balances
  -- AccountBalances $ (filter (\balance -> elem (_accountBalanceAccountId balance) (unAccountIds aIds)) (getBalances balances))

filterTransactionsByAccountIds :: AccountIds -> BulkTransactions -> BulkTransactions
filterTransactionsByAccountIds (AccountIds aIds) (BulkTransactions transactions) =
  BulkTransactions $ filter (\transaction -> elem (_bulkTransactionAccountId transaction) aIds) transactions

filterDirectDebitsByAccountIds :: AccountIds -> DirectDebitAuthorisations -> DirectDebitAuthorisations
filterDirectDebitsByAccountIds (AccountIds aIds) (DirectDebitAuthorisations dds) =
  DirectDebitAuthorisations $ filter (\dd -> elem (_accountDirectDebitAccountId dd) aIds) dds


-- This will have to take some kind of config later and the calls should actually
-- take proper inputs (like the user id for get customer). But this works well
-- enough for now.
runModelF :: Monad m => ModelF a -> m a
runModelF m = pure $ case m of
  (GetCustomer next)       -> next (CustomerPerson testPerson)
  (GetCustomerDetail next) -> next (CustomerDetailPerson testPersonDetail)
  (GetAccounts next) -> next testAccounts
  (GetBalancesAll next) -> next testBalances
  (GetBalancesForAccounts aIds next) -> next (filterBalancesByAccountIds aIds testBalances)
  (GetTransactionsAll next) -> next testAccountsTransactions
  (GetTransactionsForAccounts aIds next) -> next (filterTransactionsByAccountIds aIds testAccountsTransactions)
  (GetDirectDebitsAll next) -> next testDirectDebitAuthorisations
  (GetDirectDebitsForAccounts aIds next) -> next (filterDirectDebitsByAccountIds aIds testDirectDebitAuthorisations)
  (GetAccountById _ next) -> next testAccountDetail
  (GetTransactionsForAccount _ next) -> next testAccountTransactions
  (GetTransactionDetailForAccountTransaction _ _ next) -> next testAccountTransactionsDetail
  (GetDirectDebitsForAccount _ next) -> next testDirectDebitAuthorisations
  (GetPayeesAll _ _ _ next) -> next testPayees
  (GetPayeeDetail _ next) -> next testPayeeDetail
  (GetProductsAll _ _ _ _ _ _ next) -> next testProducts
  (GetProductDetail _ next) -> next testProductDetail
