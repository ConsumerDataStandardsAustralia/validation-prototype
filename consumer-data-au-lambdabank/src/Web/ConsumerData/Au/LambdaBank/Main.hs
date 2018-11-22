{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.LambdaBank.Main where

{--
 - Config parsing and stuff will happen here eventually. Pretty boring for now.
--}

import Web.ConsumerData.Au.Api.Types

import Text.URI                 (Authority (..))
import Text.URI.QQ              (host, scheme)

import Web.ConsumerData.Au.LambdaBank.FakeData
import Web.ConsumerData.Au.LambdaBank.Server

fakeQualifier :: Int -> LinkQualifier
fakeQualifier port = LinkQualifier
  [scheme|http|]
  (Authority
    { authUserInfo = Nothing
    , authHost     = [host|localhost|]
    , authPort     = Just $ fromIntegral port
    })
  []

main :: IO ()
main = runServer port (fakeQualifier port)
  where port = 8000
