{-# LANGUAGE QuasiQuotes       #-}
module Web.ConsumerData.Au.Api.Types.LinkTestHelpers where

import Control.Lens

import Servant.Links         (Link)
import Test.Tasty            (TestName, TestTree, testGroup)
import Test.Tasty.HUnit      (testCase, (@?=))
import Text.URI              (Authority (..), QueryParam (QueryParam), URI)
import Text.URI.Lens         (uriQuery)
import Text.URI.QQ           (host, queryKey, queryValue, scheme)

import Web.ConsumerData.Au.Api.Types

lq :: LinkQualifier
lq = LinkQualifier
  [scheme|http|]
  (Authority
    { authUserInfo = Nothing
    , authHost     = [host|localhost|]
    , authPort     = Nothing
    })
  []

linkTest :: TestName -> Link -> URI -> TestTree
linkTest tn l u = testCase tn $ linkToUri lq l @?= u

paginatedLinkTest :: TestName -> (Maybe PageNumber -> Link) -> URI -> TestTree
paginatedLinkTest tn f u = testGroup tn
  [ linkTest "No page" (f Nothing) u
  , linkTest "With Page" (f (Just $ PageNumber 2)) (u & uriQuery <>~ [QueryParam [queryKey|page|] [queryValue|2|]])
  ]
