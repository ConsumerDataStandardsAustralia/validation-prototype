pkgs: self: super:
let
  upstreamOverrides = {
    consumer-data-au-api-types = import ../../consumer-data-au-api-types/nix/consumer-data-au-api-types-overrides.nix;
    consumer-data-au-api-client = import ../../consumer-data-au-api-client/nix/consumer-data-au-api-client-overrides.nix;
  };

  bankOverrides = pkgs: self: super: {
    consumer-data-au-api-types = self.callPackage (import ../../consumer-data-au-api-types/consumer-data-au-api-types.nix) {};
    consumer-data-au-api-client = self.callPackage (import ../../consumer-data-au-api-client/consumer-data-au-api-client.nix) {};
    base-compat-batteries = pkgs.haskell.lib.dontCheck super.base-compat-batteries;
    Glob = pkgs.haskell.lib.dontCheck super.Glob;
    iproute = pkgs.haskell.lib.dontCheck super.iproute;
    mockery = pkgs.haskell.lib.dontCheck super.mockery;
    silently = pkgs.haskell.lib.dontCheck super.silently;
  };

  composedOverrides =
    pkgs.lib.foldr
      (x: acc: pkgs.lib.composeExtensions acc (x pkgs))
      (_: _: {})
      (builtins.attrValues upstreamOverrides ++ [bankOverrides]);
in
  composedOverrides self super
