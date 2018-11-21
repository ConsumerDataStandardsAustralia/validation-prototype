pkgs: self: super:
let
  upstreamSources = {
    servant-waargonaut = (import ./servant-waargonaut.nix) {nixpkgs = pkgs;};
  };

  upstreamOverrides = {
    servant-waargonaut = (import "${upstreamSources.servant-waargonaut}/servant-waargonaut-overrides.nix");
  };

  obOverrides = pkgs: (self: super: {
    servant-waargonaut = self.callPackage (import "${upstreamSources.servant-waargonaut}/servant-waargonaut.nix") {};
  });

  composedOverrides =
    pkgs.lib.foldr
      (x: acc: pkgs.lib.composeExtensions acc (x pkgs))
      (_: _: {})
      (builtins.attrValues upstreamOverrides ++ [obOverrides]);
in
  composedOverrides self super
