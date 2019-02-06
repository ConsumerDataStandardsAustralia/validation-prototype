{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
, doBench ? false
, doDocker ? false
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = import ./nix/haskellPackages.nix {inherit nixpkgs compiler;};

  consumer-data-au-lambdabank = haskellPackages.callPackage ./consumer-data-au-lambdabank.nix {};

  withBench = d: if doBench
    then pkgs.haskell.lib.doBenchmark d
    else d;

  withDocker = d: if doDocker
    then [d cdrDockerImage]
  else d;

  cdrDockerImage = pkgs.dockerTools.buildImage {
    name = "cdr-mock-server";
    tag = "latest";

    contents = [
      consumer-data-au-lambdabank
    ];

    config = {
      Version = "0.1a";
      EntryPoint = ["lambda-bank"];
      ExposedPorts = { "8080/tcp" = {}; };
    };
  };

  drv = withDocker (withBench (consumer-data-au-lambdabank) );
in
  drv
