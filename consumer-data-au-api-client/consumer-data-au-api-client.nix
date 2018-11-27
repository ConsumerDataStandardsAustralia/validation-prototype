{ mkDerivation, base, consumer-data-au-api-types, lens, servant
, servant-client, servant-client-core, stdenv
}:
mkDerivation {
  pname = "consumer-data-au-api-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base consumer-data-au-api-types lens servant servant-client
    servant-client-core
  ];
  description = "Haskell Client Api for the Australian Consumer Data Rights Specification";
  license = stdenv.lib.licenses.mit;
}
