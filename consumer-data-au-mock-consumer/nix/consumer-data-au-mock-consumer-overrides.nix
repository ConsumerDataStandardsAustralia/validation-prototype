pkgs: self: super:
let
  upstreamOverrides = {
    consumer-data-au-api-types = import ../../consumer-data-au-api-types/nix/consumer-data-au-api-types-overrides.nix;
    consumer-data-au-api-client = import ../../consumer-data-au-api-client/nix/consumer-data-au-api-client-overrides.nix;
  };

  bankOverrides = pkgs: self: super: {
    consumer-data-au-api-types = pkgs.haskell.lib.dontCheck self.callPackage (import ../../consumer-data-au-api-types/consumer-data-au-api-types.nix) {};
    consumer-data-au-api-client = pkgs.haskell.lib.dontCheck self.callPackage (import ../../consumer-data-au-api-client/consumer-data-au-api-client.nix) {};

    # HPack just won't work in ghcjs atm because of the yaml lib at the moment. Let's cheat.
    hpack = pkgs.haskellPackages.hpack;

    base-compat-batteries = pkgs.haskell.lib.dontCheck super.base-compat-batteries;
    Glob = pkgs.haskell.lib.dontCheck super.Glob;
    iproute = pkgs.haskell.lib.dontCheck super.iproute;
    mockery = pkgs.haskell.lib.dontCheck super.mockery;
    silently = pkgs.haskell.lib.dontCheck super.silently;
    bifunctors = pkgs.haskell.lib.dontCheck super.bifunctors;
    unix-time = pkgs.haskell.lib.dontCheck super.unix-time;
    markdown-unlit = pkgs.haskell.lib.dontCheck super.markdown-unlit;
    http-date = pkgs.haskell.lib.dontCheck super.http-date;
    unliftio = pkgs.haskell.lib.dontCheck super.unliftio;
    hw-hspec-hedgehog = pkgs.haskell.lib.dontCheck super.hw-hspec-hedgehog;
    bsb-hw-chunked = pkgs.haskell.lib.dontCheck super.bsb-hw-chunked;
    bsb-http-chunked = pkgs.haskell.lib.dontCheck super.bsb-http-chunked;
    bits-extra = pkgs.haskell.lib.dontCheck super.bits-extra;
    zippers = pkgs.haskell.lib.dontCheck super.zippers;
    monad-par = pkgs.haskell.lib.dontCheck super.monad-par;
    hw-prim = pkgs.haskell.lib.dontCheck super.hw-prim;
    hw-rankselect-base = pkgs.haskell.lib.dontCheck super.hw-rankselect-base;
    hw-rankselect = pkgs.haskell.lib.dontCheck super.hw-rankselect;
    conduit = pkgs.haskell.lib.dontCheck super.conduit;
    aeson-diff = pkgs.haskell.lib.dontCheck super.aeson-diff;
    http2 = pkgs.haskell.lib.dontCheck super.http2;
    yaml = pkgs.haskell.lib.dontCheck super.yaml;
    wai-extra = pkgs.haskell.lib.dontCheck super.wai-extra;
    wai-app-static = pkgs.haskell.lib.dontCheck super.wai-app-static;
    servant = pkgs.haskell.lib.dontCheck super.servant;
    servant-server = pkgs.haskell.lib.dontCheck super.servant-server;
    servant-client = pkgs.haskell.lib.dontCheck super.servant-client;
    waargonaut = pkgs.haskell.lib.dontCheck super.acme-default;
  };

  composedOverrides =
    pkgs.lib.foldr
      (x: acc: pkgs.lib.composeExtensions acc (x pkgs))
      (_: _: {})
      (builtins.attrValues upstreamOverrides ++ [bankOverrides]);
in
  composedOverrides self super
