{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
, doBench ? false
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = import ./nix/haskellPackages.nix {inherit nixpkgs compiler;};

  consumer-data-au-api-client = haskellPackages.callPackage ./consumer-data-au-api-client.nix {};

  withBench = d: if doBench
    then pkgs.haskell.lib.doBenchmark d
    else d;

  drv = withBench (consumer-data-au-api-client);
in
  drv
