{ mkDerivation, aeson, base, bytestring
, consumer-data-au-api-client, consumer-data-au-api-types, country
, exceptions, hedgehog, http-client, lens, modern-uri, profunctors
, servant, servant-client, servant-server, stdenv, tasty
, tasty-discover, tasty-golden, tasty-hedgehog, tasty-hunit, text
, time, transformers, wai, warp
}:
mkDerivation {
  pname = "consumer-data-au-lambdabank";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring consumer-data-au-api-client
    consumer-data-au-api-types country lens modern-uri profunctors
    servant servant-server text time wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base consumer-data-au-api-client consumer-data-au-api-types
    exceptions hedgehog http-client lens modern-uri servant-client
    tasty tasty-discover tasty-golden tasty-hedgehog tasty-hunit text
    transformers
  ];
  testToolDepends = [ tasty-discover ];
  description = "Mock bank for the Australian Consumer Data Rights Specification";
  license = stdenv.lib.licenses.mit;
}
