{ nixpkgs , compiler }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  bankOverrides = import ./consumer-data-au-api-bank-overrides.nix pkgs;

  bankHaskellPackages = haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_:_: {}))
      bankOverrides;
  });
in
  bankHaskellPackages
