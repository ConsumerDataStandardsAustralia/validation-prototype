{ mkDerivation, aeson, base, bytestring
, consumer-data-au-api-client, consumer-data-au-api-types, country
, currency-codes, digit, exceptions, free, hedgehog, http-client
, jose, lens, modern-uri, mtl, profunctors, servant, servant-client
, servant-server, stdenv, stm, tasty, tasty-discover, tasty-golden
, tasty-hedgehog, tasty-hunit, text, time, transformers, wai, warp
}:
mkDerivation {
  pname = "consumer-data-au-lambdabank";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring consumer-data-au-api-client
    consumer-data-au-api-types country currency-codes digit free jose
    lens modern-uri mtl profunctors servant servant-server stm text
    time transformers wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base consumer-data-au-api-client consumer-data-au-api-types
    exceptions free hedgehog http-client lens modern-uri servant-client
    tasty tasty-discover tasty-golden tasty-hedgehog tasty-hunit text
    transformers
  ];
  testToolDepends = [ tasty-discover ];
  description = "Mock bank for the Australian Consumer Data Rights Specification";
  license = stdenv.lib.licenses.mit;
}
