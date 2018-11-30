module Country.Gens where

import Country
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

-- Caution is required around enumBounded, as it does not always do what one might want.
-- I have inspected the Enum and Bounded instances from Country. They look
-- reasonable and compatible with enumBounded.
countryGen :: Gen Country
countryGen = Gen.enumBounded
