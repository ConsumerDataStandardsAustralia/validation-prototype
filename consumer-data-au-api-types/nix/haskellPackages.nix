{ nixpkgs , compiler }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  obOverrides = import ./consumer-data-au-api-types-overrides.nix pkgs;

  obHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_:_: {}))
      obOverrides;
  });
in
  obHaskellPackages
