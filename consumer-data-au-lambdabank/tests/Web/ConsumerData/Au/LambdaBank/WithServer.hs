{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.WithServer where

import Control.Concurrent  (forkIO, killThread)
import Control.Exception   (bracket, throwIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
    (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientM, Scheme (Http),
    runClientM)

import Web.ConsumerData.Au.LambdaBank.Main

withServer :: Int -> ClientM () -> IO ()
withServer p f = bracket (forkIO $ runServer p) killThread . const $ do
  m <- newManager defaultManagerSettings
  let env = ClientEnv m (BaseUrl Http "localhost" p "") Nothing
  runClientM f env >>= either throwIO pure
