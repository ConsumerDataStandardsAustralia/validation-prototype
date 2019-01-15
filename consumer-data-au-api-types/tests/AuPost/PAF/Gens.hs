module AuPost.PAF.Gens where

import AuPost.PAF
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen


flatTypeGen :: Gen FlatType
flatTypeGen = Gen.enumBounded

floorTypeGen :: Gen FloorType
floorTypeGen = Gen.enumBounded

postalDeliveryTypeGen :: Gen PostalDeliveryType
postalDeliveryTypeGen = Gen.enumBounded

stateTypeGen :: Gen StateType
stateTypeGen = Gen.enumBounded

streetSuffixGen :: Gen StreetSuffix
streetSuffixGen = Gen.enumBounded

streetTypeGen :: Gen StreetType
streetTypeGen = Gen.enumBounded
