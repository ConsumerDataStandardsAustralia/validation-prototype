{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.ConsumerData.Au.Api.Types.Auth.Gens where

import qualified Data.Dependent.Map as DM
import           Data.Dependent.Sum (DSum ((:=>)))

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.JOSE.JWK        as JWK
import           Hedgehog               (MonadGen)
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range

import Web.ConsumerData.Au.Api.Types.Auth.AuthorisationRequest (Claims (Claims))
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

genJWK ::
  ( MonadGen n
  , MonadIO n
  )
  => n JWK.JWK
genJWK =
  genKeyMaterial >>= liftIO . JWK.genJWK

-- | Valid key material dictated by allowed signing algorithms (see 'signingAlg') and the
-- <https://github.com/frasertweedale/hs-jose/blob/18865d7af9d3b16d737f38579643399cf4facc1b/src/Crypto/JOSE/JWA/JWK.hs#L610 JWK module in @jose@>
genKeyMaterial ::
  MonadGen n
  => n JWK.KeyMaterialGenParam
genKeyMaterial =
  Gen.choice
    [ pure (JWK.ECGenParam JWK.P_256)
    , JWK.RSAGenParam <$> Gen.int (Range.linear (2048 `div` 8) (4096 `div` 8))
    ]
