module Hedgehog.Helpers where

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Maybe
import qualified Hedgehog.Internal.Gen     as Gen
import qualified Hedgehog.Internal.Seed    as Seed
import qualified Hedgehog.Internal.Tree    as Tree

sampleT :: (MonadIO m) => Gen.GenT m a  -> m a
sampleT gen =
    let
      loop n =
        if n <= 0 then
          error "Hedgehog.Helpers.sampleT: too many discards, could not generate a sample"
        else do
          seed <- Seed.random
          mX <- runMaybeT . Tree.runTree $ Gen.runGenT 30 seed gen
          case mX of
            Nothing ->
              loop (n - 1)
            Just x ->
              pure $ Tree.nodeValue x
    in
      loop (100 :: Int)

