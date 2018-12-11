module Web.ConsumerData.Au.Api.Types.Banking.AccountsGens where

import Hedgehog

import Web.ConsumerData.Au.Api.Types
import Web.ConsumerData.Au.Api.Types.Data.CommonFieldTypesGens


accountIdGen :: Gen AccountId
accountIdGen = AccountId <$> asciiStringGen
