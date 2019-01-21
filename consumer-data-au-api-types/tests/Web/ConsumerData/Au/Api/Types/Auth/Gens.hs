{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.ConsumerData.Au.Api.Types.Auth.Gens where

import           Control.Lens                                            ((^.))
import           Control.Monad.Catch
    (MonadThrow, throwM)
import           Control.Monad.Except
    (MonadIO, liftIO, runExceptT)
import           Crypto.JOSE
    (Alg (..))
import qualified Crypto.JOSE.JWK                                         as JWK
import qualified Data.Dependent.Map                                      as DM
import           Data.Dependent.Sum
    (DSum ((:=>)))
import           Hedgehog
    (MonadGen)
import qualified Hedgehog.Gen                                            as Gen
import qualified Hedgehog.Range                                          as Range
import           Text.URI
    (Authority, RText, RTextLabel (Scheme), URI (URI), mkScheme)
import           Text.URI.Gens
    (genAuthority, genPathPieces)
import           Web.ConsumerData.Au.Api.Types.Auth.AuthorisationRequest
    (Claims (Claims))
import           Web.ConsumerData.Au.Api.Types.Auth.Common.Common
    (Acr (Acr), Claim (Claim), HttpsUrl, TokenSubject (..), mkHttpsUrl, ConsentId (..))
import           Web.ConsumerData.Au.Api.Types.Auth.Common.IdToken
    (IdToken (IdToken), IdTokenClaims, IdTokenKey (..))
import           Web.ConsumerData.Au.Api.Types.Auth.Error

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
    IdToken Nothing Nothing (Claim [Acr "foo"] True) (Claim [ConsentId "fake consent id"] True) <$> (DM.fromList <$> gens)
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
  => n (JWK.JWK, Alg)
genJWK = do
  jwk <- genKeyMaterial >>= liftIO . JWK.genJWK
  let alg = signingAlg (jwk ^. JWK.jwkMaterial)
  return (jwk,alg)

-- | Valid signing algorithms are specified in
-- <https://openid.net/specs/openid-financial-api-part-2.html#jws-algorithm-considerations FAPI R+W §8.6>.
-- These choices dictate the key material that is valid --- see 'genKeyMaterial'
signingAlg ::
  JWK.KeyMaterial
  -> Alg
signingAlg = \case
  JWK.ECKeyMaterial _ -> ES256
  _ -> PS256

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

genUrls ::
  ( MonadGen n
  , MonadThrow n
  )
  => RText 'Scheme -> Authority -> n [URI]
genUrls scheme auth = Gen.list (Range.linear 1 10) $ genUrl scheme auth

genUrl ::
  ( MonadGen n
  , MonadThrow n
  )
  => RText 'Scheme -> Authority -> n URI
genUrl scheme auth = URI
    <$> pure (Just scheme)
    <*> pure (Right auth)
    <*> genPathPieces
    <*> pure []
    <*> pure Nothing

genHttpsUrl ::
  ( MonadGen n
  , MonadThrow n
  )
  => n HttpsUrl
genHttpsUrl = do
  https <- mkScheme "https"
  autho <- genAuthority
  uri <- genUrl https autho
  h :: Either HttpsUrlError HttpsUrl <- runExceptT $ mkHttpsUrl uri
  either throwM pure h
