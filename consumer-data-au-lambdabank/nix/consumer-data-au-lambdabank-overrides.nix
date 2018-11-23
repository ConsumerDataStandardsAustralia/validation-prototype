pkgs: self: super:
let
  upstreamOverrides = {
    consumer-data-au-api-types = import ../../consumer-data-au-api-types/nix/consumer-data-au-api-types-overrides.nix;
  };

  bankOverrides = pkgs: self: super: {
    consumer-data-au-api-types = self.callPackage (import ../../consumer-data-au-api-types/consumer-data-au-api-types.nix) {};
  };

  composedOverrides =
    pkgs.lib.foldr
      (x: acc: pkgs.lib.composeExtensions acc (x pkgs))
      (_: _: {})
      (builtins.attrValues upstreamOverrides ++ [bankOverrides]);
in
  composedOverrides self super
