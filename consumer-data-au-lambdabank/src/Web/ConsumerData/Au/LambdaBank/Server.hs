{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Web.ConsumerData.Au.LambdaBank.Server where

{--
Notes: this is just moving the FakeServer from the types tests across for now.
We will split it apart and make it into a real server with config and a database
and not all in one file. Don't be too upset
that it looks heinous for now!
--}

import Control.Lens
import Web.ConsumerData.Au.Api.Types

import Control.Concurrent       (forkIO, killThread)
import Control.Exception        (bracket, throwIO)
import Control.Monad.Reader     (runReader)
import Country.Identifier       (australia)
import Data.List.NonEmpty       (NonEmpty ((:|)))
import Data.Maybe               (fromMaybe)
import Data.Profunctor          (lmap)
import Data.Proxy               (Proxy (Proxy))
import Data.Time                (UTCTime (..), fromGregorian)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Servant.API.Generic
    (AsApi, GenericServant, ToServant, ToServantApi, genericApi)
import Servant.Links            (Link)
import Servant.Server
    (Handler, HasServer, ServerT, hoistServer, serve)
import Servant.Server.Generic   (AsServerT, genericServerT)
import Text.URI                 (Authority (..))
import Text.URI.QQ              (host, scheme)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.Server.Banking  (bankingServer)
import Web.ConsumerData.Au.LambdaBank.Server.Common   (commonServer)
import Web.ConsumerData.Au.LambdaBank.Server.Internal (LambdaBankM)

routes :: Api (AsServerT LambdaBankM)
routes = Api
  { _common = commonServer
  , _banking = bankingServer
  }

app :: LinkQualifier -> Application
app lq = genericServeT (runLambdaBankM lq) routes
  where
    runLambdaBankM :: LinkQualifier -> LambdaBankM a -> Handler a
    runLambdaBankM lq m = pure $ runReader m lq

-- | (Remove this when we go to servant 0.15)
--   Transform a record of routes with custom monad into a WAI 'Application',
--   by providing a transformation to bring each handler back in the 'Handler'
--   monad.
genericServeT
  :: forall (routes :: * -> *) (m :: * -> *).
     ( GenericServant routes (AsServerT m)
     , GenericServant routes AsApi
     , HasServer (ToServantApi routes) '[]
     , ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
     )
  => (forall a. m a -> Handler a) -- ^ 'hoistServer' argument to come back to 'Handler'
  -> routes (AsServerT m)         -- ^ your record full of request handlers
  -> Application
genericServeT f server = serve p $ hoistServer p f (genericServerT server)
  where
    p = genericApi (Proxy :: Proxy routes)

runServer :: Int -> LinkQualifier -> IO ()
runServer port lq = run port (app $ lq)
