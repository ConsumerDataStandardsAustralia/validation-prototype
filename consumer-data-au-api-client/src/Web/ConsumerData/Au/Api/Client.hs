module Web.ConsumerData.Au.Api.Client
( apiClient
, commonClient
, customerClient
, bankingClient
, bankingAccountsClient
, accountsByIdClient
, payeesClient
) where

import Control.Lens           (Getter, to)
import Servant.API.Generic    (fromServant)
import Servant.Client         (ClientM, client)
import Servant.Client.Generic (AsClientT)

import Web.ConsumerData.Au.Api.Types

apiClient :: Api (AsClientT ClientM)
apiClient = fromServant $ client api

type GenericClient t = t (AsClientT ClientM)

commonClient :: Getter (GenericClient Api) (GenericClient CommonApi)
commonClient = common . to fromServant

customerClient :: Getter (GenericClient CommonApi) (GenericClient CustomerApi)
customerClient = customer . to fromServant

bankingClient :: Getter (GenericClient Api) (GenericClient BankingApi)
bankingClient = banking . to fromServant

bankingAccountsClient :: Getter (GenericClient BankingApi) (GenericClient AccountsApi)
bankingAccountsClient = bankingAccounts . to fromServant

accountsByIdClient :: Getter (GenericClient AccountsApi) (AccountId -> GenericClient AccountApi)
accountsByIdClient = accountsById . to (\r i -> fromServant (r i))

payeesClient :: Getter (GenericClient BankingApi) (GenericClient PayeesApi)
payeesClient = bankingPayees . to fromServant
