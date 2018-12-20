module Web.ConsumerData.Au.LambdaBank.Server.Internal where

import Control.Monad.Reader          (ReaderT, asks)
import Servant.Links                 (Link)
import Web.ConsumerData.Au.Api.Types
    (LinkQualifier, PaginatedResponse, Paginator, StandardResponse,
    mkPaginatedResponse, mkStandardResponse)
import Web.ConsumerData.Au.LambdaBank.Server.State (LbState)

import Web.ConsumerData.Au.LambdaBank.Model

type LambdaBankM = ReaderT LbState ModelM

bankStandardResponse :: a -> Link -> LambdaBankM (StandardResponse a)
bankStandardResponse a l = asks $ \lq -> mkStandardResponse a lq l

bankPaginatedResponse :: a -> Paginator -> LambdaBankM (PaginatedResponse a)
bankPaginatedResponse a p = asks $ \lq -> mkPaginatedResponse a lq p
