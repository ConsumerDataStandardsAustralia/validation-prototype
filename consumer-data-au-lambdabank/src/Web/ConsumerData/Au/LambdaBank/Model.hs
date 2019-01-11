{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.ConsumerData.Au.LambdaBank.Model where

import Web.ConsumerData.Au.Api.Types

import Data.Text                 (Text)
import Control.Monad.Free        (MonadFree, liftF)
import Control.Monad.Free.Church (F, iterM)

import Web.ConsumerData.Au.LambdaBank.FakeData

data ModelF next where
  GetCustomer                               :: (CustomerResponse           -> next) -> ModelF next
  GetCustomerDetail                         :: (CustomerDetailResponse     -> next) -> ModelF next
  GetAccounts                               :: (Accounts  -> next) -> ModelF next
  GetBalancesAll                            :: (AccountBalances -> next) -> ModelF next
  GetBalancesForAccounts                    :: AccountIds -> (AccountBalances -> next) -> ModelF next
  GetTransactionsAll                        :: (Transactions -> next) -> ModelF next
  GetTransactionsForAccounts                :: AccountIds -> (Transactions -> next) -> ModelF next
  GetDirectDebitsAll                        :: (DirectDebitAuthorisations -> next) -> ModelF next
  GetDirectDebitsForAccounts                :: AccountIds -> (DirectDebitAuthorisations -> next) -> ModelF next
  GetAccountById                            :: AccountId -> (AccountDetail -> next) -> ModelF next
  GetTransactionsForAccount                 :: AccountId -> (Transactions -> next) -> ModelF next
  GetTransactionDetailForAccountTransaction :: AccountId -> TransactionId -> (TransactionDetailResponse -> next) -> ModelF next
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
    -> Maybe EnumProductCategory
    -> Maybe PageNumber
    -> Maybe PageSize
    -> (Products -> next) -> ModelF next
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

getBalancesForAccounts :: MonadFree ModelF m => AccountIds -> m AccountBalances
getBalancesForAccounts aIds = liftF $ GetBalancesForAccounts aIds id

getTransactionsAll :: MonadFree ModelF m => m Transactions
getTransactionsAll = liftF $ GetTransactionsAll id

getTransactionsForAccounts :: MonadFree ModelF m => AccountIds -> m Transactions
getTransactionsForAccounts aIds = liftF $ GetTransactionsForAccounts aIds id

getDirectDebitsAll :: MonadFree ModelF m => m DirectDebitAuthorisations
getDirectDebitsAll = liftF $ GetDirectDebitsAll id

getDirectDebitsForAccounts :: MonadFree ModelF m => AccountIds -> m DirectDebitAuthorisations
getDirectDebitsForAccounts aIds = liftF $ GetDirectDebitsForAccounts aIds id

getAccountById :: MonadFree ModelF m => AccountId -> m AccountDetail
getAccountById accountId = liftF $ GetAccountById accountId id

getTransactionsForAccount :: MonadFree ModelF m => AccountId -> m Transactions
getTransactionsForAccount accountId = liftF $ GetTransactionsForAccount accountId id

getTransactionDetailForAccountTransaction :: MonadFree ModelF m => AccountId -> TransactionId -> m TransactionDetailResponse
getTransactionDetailForAccountTransaction aId xactId = liftF $ GetTransactionDetailForAccountTransaction aId xactId id

getDirectDebitsForAccount :: MonadFree ModelF m => AccountId -> m DirectDebitAuthorisations
getDirectDebitsForAccount accountId = liftF $ GetDirectDebitsForAccount accountId id

getPayeesAll :: MonadFree ModelF m => Maybe PayeeType -> Maybe PageNumber -> Maybe PageSize -> m Payees
getPayeesAll pt pn ps = liftF $ GetPayeesAll pt pn ps id

getPayeeDetail :: MonadFree ModelF m => PayeeId -> m PayeeDetail
getPayeeDetail pId = liftF $ GetPayeeDetail pId id

getProductsAll
  :: MonadFree ModelF m
  => Maybe ProductEffective
  -> Maybe DateTimeString
  -> Maybe Text
  -> Maybe EnumProductCategory
  -> Maybe PageNumber
  -> Maybe PageSize
  -> m Products
getProductsAll pe dts t pc pn ps = liftF $ GetProductsAll pe dts t pc pn ps id

getProductDetail :: MonadFree ModelF m => ProductId -> m ProductDetail
getProductDetail pId = liftF $ GetProductDetail pId id


filterBalancesByAccountIds :: AccountIds -> AccountBalances -> AccountBalances
filterBalancesByAccountIds (AccountIds aIds) (AccountBalances balances) =
  AccountBalances $ filter (\balance -> elem (_accountBalanceAccountId balance) aIds) balances
  -- AccountBalances $ (filter (\balance -> elem (_accountBalanceAccountId balance) (unAccountIds aIds)) (getBalances balances))

filterTransactionsByAccountIds :: AccountIds -> Transactions -> Transactions
filterTransactionsByAccountIds (AccountIds aIds) (Transactions transactions) =
  Transactions $ filter (\transaction -> elem (_transactionAccountId transaction) aIds) transactions

filterDirectDebitsByAccountIds :: AccountIds -> DirectDebitAuthorisations -> DirectDebitAuthorisations
filterDirectDebitsByAccountIds (AccountIds aIds) (DirectDebitAuthorisations dds) =
  DirectDebitAuthorisations $ filter (\dd -> elem (_accountDirectDebitAccountId dd) aIds) dds


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
  (GetBalancesForAccounts aIds next) -> next (filterBalancesByAccountIds aIds testBalances)
  (GetTransactionsAll next) -> next testTransactions
  (GetTransactionsForAccounts aIds next) -> next (filterTransactionsByAccountIds aIds testTransactions)
  (GetDirectDebitsAll next) -> next testDirectDebitAuthorisations
  (GetDirectDebitsForAccounts aIds next) -> next (filterDirectDebitsByAccountIds aIds testDirectDebitAuthorisations)
  (GetAccountById _ next) -> next testAccountDetail
  (GetTransactionsForAccount _ next) -> next testTransactions
  (GetTransactionDetailForAccountTransaction _ _ next) -> next testTransactionDetailResponse
  (GetDirectDebitsForAccount _ next) -> next testDirectDebitAuthorisations
  (GetPayeesAll _ _ _ next) -> next testPayees
  (GetPayeeDetail _ next) -> next testPayeeDetail
  (GetProductsAll _ _ _ _ _ _ next) -> next testProducts
  (GetProductDetail _ next) -> next testProductDetail
