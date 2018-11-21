{ mkDerivation, aeson, base, bytestring, consumer-data-au-api-types
, hedgehog, hspec, servant, servant-server, stdenv, tasty
, tasty-discover, tasty-golden, tasty-hedgehog, tasty-hunit
}:
mkDerivation {
  pname = "consumer-data-au-api-bank";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring consumer-data-au-api-types servant
    servant-server
  ];
  testHaskellDepends = [
    aeson base bytestring hedgehog hspec servant servant-server tasty
    tasty-discover tasty-golden tasty-hedgehog tasty-hunit
  ];
  description = "Mock bank for the Australian Consumer Data Rights Specification";
  license = stdenv.lib.licenses.mit;
}
