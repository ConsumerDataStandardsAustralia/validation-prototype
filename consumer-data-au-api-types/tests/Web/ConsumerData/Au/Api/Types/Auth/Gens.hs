{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.ConsumerData.Au.Api.Types.Auth.Gens where

import qualified Data.Dependent.Map as DM
import           Data.Dependent.Sum (DSum ((:=>)))

import           Hedgehog       (MonadGen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Web.ConsumerData.Au.Api.Types.Auth.AuthorisationRequest
    (Claims (Claims))
import Web.ConsumerData.Au.Api.Types.Auth.Common.Common
    (Acr (Acr), Claim (Claim), TokenSubject (..))
import Web.ConsumerData.Au.Api.Types.Auth.Common.IdToken
    (IdToken (IdToken), IdTokenClaims, IdTokenKey (..))

genIdTokenClaims ::
  forall n.
  MonadGen n
  => n IdTokenClaims
genIdTokenClaims =
  let
    genSub ::
      n (Claim TokenSubject)
    genSub =
      (\a -> Claim [a] False) . TokenSubject <$> Gen.text (Range.linear 1 5) Gen.alphaNum
    gens :: n [DSum IdTokenKey Claim]
    gens =
      sequence
      [
        (IdTokenSub :=>) <$> genSub
      -- , IdTokenSub
      ]
  in
    IdToken Nothing Nothing (Claim [Acr "foo"] True) <$> (DM.fromList <$> gens)
      -- DM.traverseWithKey (const (fmap pure)) (DM.fromList <$> gens)

genClaims ::
  MonadGen n
  => n Claims
genClaims =
  Claims Nothing <$> genIdTokenClaims
