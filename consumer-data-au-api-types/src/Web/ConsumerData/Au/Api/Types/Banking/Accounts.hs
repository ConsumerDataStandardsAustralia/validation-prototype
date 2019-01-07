{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Web.ConsumerData.Au.Api.Types.Banking.Accounts
  ( module Web.ConsumerData.Au.Api.Types.Banking.Accounts
  ) where

import Control.Lens                        (Getter, to)
import Data.Text                           (Text)
import GHC.Generics                        (Generic)
import Servant.API
    ((:>), Capture, FromHttpApiData, Get, Post, QueryParam, ToHttpApiData,
    parseQueryParam, toQueryParam)
import Servant.API.ContentTypes.Waargonaut (WaargJSON)
import Servant.API.Generic                 ((:-), AsApi, ToServant, fromServant)
import Servant.Links                       (AsLink, Link)

import Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDetail
import Web.ConsumerData.Au.Api.Types.Banking.Common.AccountDirectDebit
    (DirectDebitAuthorisations)
import Web.ConsumerData.Au.Api.Types.Banking.Common.Accounts
import Web.ConsumerData.Au.Api.Types.Banking.Common.AccountTransactions
import Web.ConsumerData.Au.Api.Types.Banking.Common.BulkTransaction
import Web.ConsumerData.Au.Api.Types.Banking.Common.Products
import Web.ConsumerData.Au.Api.Types.Banking.Common.Transaction
    (TransactionId)
import Web.ConsumerData.Au.Api.Types.Banking.Common.TransactionsDetail
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypes
import Web.ConsumerData.Au.Api.Types.Response
import Web.ConsumerData.Au.Api.Types.Tag

data AccountOpenStatus = AccountOpen | AccountClosed | AccountOpenStatusAll deriving (Eq)

instance ToHttpApiData AccountOpenStatus where
  toQueryParam AccountOpen          = "OPEN"
  toQueryParam AccountClosed        = "CLOSED"
  toQueryParam AccountOpenStatusAll = "ALL"

instance FromHttpApiData AccountOpenStatus where
  parseQueryParam "OPEN"   = Right AccountOpen
  parseQueryParam "CLOSED" = Right AccountClosed
  parseQueryParam "ALL"    = Right AccountOpenStatusAll
  parseQueryParam t        = Left $ "Invalid AccountOpenStatus: " <> t

data AccountIsOwned = AccountOwned | AccountNotOwned | AccountIsOwnedAll deriving (Eq)

instance ToHttpApiData AccountIsOwned where
  toQueryParam AccountOwned      = "OWNED"
  toQueryParam AccountNotOwned   = "NOT_OWNED"
  toQueryParam AccountIsOwnedAll = "ALL"

instance FromHttpApiData AccountIsOwned where
  parseQueryParam "OWNED"     = Right AccountOwned
  parseQueryParam "NOT_OWNED" = Right AccountNotOwned
  parseQueryParam "ALL"       = Right AccountIsOwnedAll
  parseQueryParam t           = Left $ "Invalid AccountIsOwned: " <> t

type AccountOpenStatusParam = QueryParam "open-status" AccountOpenStatus
type AccountIsOwnedParam = QueryParam "is-owned" AccountIsOwned
type AccountProductCategoryParam = QueryParam "product-category" ProductCategory
type TransactionStartTimeParam = QueryParam "start-time" DateTimeString
type TransactionEndTimeParam = QueryParam "end-time" DateTimeString
type TransactionMinAmountParam = QueryParam "min-amount" AmountString
type TransactionMaxAmountParam = QueryParam "max-amount" AmountString
type TransactionTextParam = QueryParam "text" Text

type AccountsGetRoute r = r :-
  ( AccountOpenStatusParam
  :> AccountIsOwnedParam
  :> AccountProductCategoryParam
  :> PaginatedRoute (Get '[WaargJSON OB] AccountsGetResponse)
  )
type AccountsBalancesRoute r e = r :- "balances" :> e
type AccountsBalancesGetRoute r = AccountsBalancesRoute r
  ( AccountOpenStatusParam
  :> AccountIsOwnedParam
  :> AccountProductCategoryParam
  :> PaginatedRoute (Get '[WaargJSON OB] AccountBulkBalanceResponse)
  )
type AccountsBalancesPostRoute r = AccountsBalancesRoute r (Post '[WaargJSON OB] AccountBalanceByIdsResponse)
type AccountsTransactionsRoute r e = r :- "transactions" :> e
type AccountsTransactionsGetRoute r = AccountsTransactionsRoute r
  (  TransactionStartTimeParam
  :> TransactionEndTimeParam
  :> TransactionMinAmountParam
  :> TransactionMaxAmountParam
  :> TransactionTextParam
  :> AccountOpenStatusParam
  :> AccountIsOwnedParam
  :> AccountProductCategoryParam
  :> PaginatedRoute (Get '[WaargJSON OB] AccountsTransactionsResponse)
  )
type AccountsTransactionsPostRoute r = AccountsTransactionsRoute r
  (  TransactionStartTimeParam
  :> TransactionEndTimeParam
  :> TransactionMinAmountParam
  :> TransactionMaxAmountParam
  :> TransactionTextParam
  :> (PaginatedRoute (Post '[WaargJSON OB] AccountsTransactionsResponse))
  )
type AccountsDirectDebitsRoute r e = r :- "direct-debits" :> e
type AccountsDirectDebitsGetRoute r = AccountsDirectDebitsRoute r
  (  AccountIsOwnedParam
  :> AccountProductCategoryParam
  :> PaginatedRoute (Get '[WaargJSON OB] AccountDirectDebitsResponse))
type AccountsDirectDebitsPostRoute r = AccountsDirectDebitsRoute r (Post '[WaargJSON OB] AccountDirectDebitsResponse)
type AccountsByIdRoute r = r :- Capture "accountId" AccountId :> ToServant AccountApi AsApi

data AccountsApi r = AccountsApi
  { _accountsGet              :: AccountsGetRoute r
  , _accountsBalancesGet      :: AccountsBalancesGetRoute r
  , _accountsBalancesPost     :: AccountsBalancesPostRoute r
  , _accountsTransactionsGet  :: AccountsTransactionsGetRoute r
  , _accountsTransactionsPost :: AccountsTransactionsPostRoute r
  , _accountsDirectDebitsGet  :: AccountsDirectDebitsGetRoute r
  , _accountsDirectDebitsPost :: AccountsDirectDebitsPostRoute r
  , _accountsById             :: AccountsByIdRoute r
  } deriving (Generic)

accountsGet :: Getter (AccountsApi r) (AccountsGetRoute r)
accountsGet = to _accountsGet

accountsBalancesGet :: Getter (AccountsApi r) (AccountsBalancesGetRoute r)
accountsBalancesGet = to _accountsBalancesGet
accountsBalancesPost :: Getter (AccountsApi r) (AccountsBalancesPostRoute r)
accountsBalancesPost = to _accountsBalancesPost
accountsTransactionsGet :: Getter (AccountsApi r) (AccountsTransactionsGetRoute r)
accountsTransactionsGet = to _accountsTransactionsGet
accountsTransactionsPost :: Getter (AccountsApi r) (AccountsTransactionsPostRoute r)
accountsTransactionsPost = to _accountsTransactionsPost
accountsDirectDebitsGet :: Getter (AccountsApi r) (AccountsDirectDebitsGetRoute r)
accountsDirectDebitsGet = to _accountsDirectDebitsGet
accountsDirectDebitsPost :: Getter (AccountsApi r) (AccountsDirectDebitsPostRoute r)
accountsDirectDebitsPost = to _accountsDirectDebitsPost

accountsById :: Getter (AccountsApi r) (AccountsByIdRoute r)
accountsById = to _accountsById
accountsByIdLinks :: Getter (AccountsApi (AsLink Link)) (AccountId -> AccountApi (AsLink Link))
accountsByIdLinks = accountsById . to (\r i -> fromServant (r i))

type AccountGetRoute r = r :- Get '[WaargJSON OB] AccountByIdResponse
type AccountTransactionRoute r e = r :- "transactions" :> e
type AccountTransactionsGetRoute r = AccountTransactionRoute r
  (  TransactionStartTimeParam
  :> TransactionEndTimeParam
  :> TransactionMinAmountParam
  :> TransactionMaxAmountParam
  :> TransactionTextParam
  :> PaginatedRoute (Get '[WaargJSON OB] AccountTransactionsResponse)
  )
type AccountTransactionByIdGetRoute r = AccountTransactionRoute r (Capture "transactionId" TransactionId :> Get '[WaargJSON OB] AccountTransactionDetailResponse)
type AccountDirectDebitsGetRoute r = r :- "direct-debits" :> (PaginatedRoute (Get '[WaargJSON OB] AccountDirectDebitsResponse))

data AccountApi r = AccountApi
  { _accountGet                :: AccountGetRoute r
  , _accountTransactionsGet    :: AccountTransactionsGetRoute r
  , _accountTransactionByIdGet :: AccountTransactionByIdGetRoute r
  , _accountDirectDebitsGet    :: AccountDirectDebitsGetRoute r
  } deriving (Generic)

accountGet :: Getter (AccountApi r) (AccountGetRoute r)
accountGet = to _accountGet
accountTransactionsGet :: Getter (AccountApi r) (AccountTransactionsGetRoute r)
accountTransactionsGet = to _accountTransactionsGet
accountTransactionByIdGet :: Getter (AccountApi r) (AccountTransactionByIdGetRoute r)
accountTransactionByIdGet = to _accountTransactionByIdGet
accountDirectDebitsGet :: Getter (AccountApi r) (AccountDirectDebitsGetRoute r)
accountDirectDebitsGet = to _accountDirectDebitsGet

type AccountsGetResponse = PaginatedResponse Accounts
type AccountByIdResponse = StandardResponse AccountDetail
type AccountBulkBalanceResponse = PaginatedResponse AccountBalances
type AccountBalanceByIdsResponse = StandardResponse AccountBalances
type AccountDirectDebitsResponse = PaginatedResponse DirectDebitAuthorisations
type AccountTransactionsResponse = PaginatedResponse AccountTransactions
type AccountTransactionDetailResponse = StandardResponse TransactionsDetail
type AccountsTransactionsResponse = PaginatedResponse BulkTransactions
