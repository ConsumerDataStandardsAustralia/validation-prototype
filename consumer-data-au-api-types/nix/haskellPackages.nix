{ nixpkgs , compiler }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  upstreamSources = {
    servant-waargonaut = (import ./servant-waargonaut.nix) { inherit nixpkgs; };
  };
  
  upstreamOverrides = {
    servant-waargonaut = (import "${upstreamSources.servant-waargonaut}/servant-waargonaut-overrides.nix");
  };

  obOverrides = pkgs: (self: super: {
    servant-waargonaut = self.callPackage (import "${upstreamSources.servant-waargonaut}/servant-waargonaut.nix") {};
  });

  obHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.foldr 
      (x: acc: pkgs.lib.composeExtensions acc (x { inherit pkgs; }))
      (old.overrides or (_: _: {}))
      [upstreamOverrides.servant-waargonaut obOverrides];
  });
in
  obHaskellPackages
