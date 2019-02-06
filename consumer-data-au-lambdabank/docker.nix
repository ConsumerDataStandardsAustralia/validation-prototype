{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
}:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = import ./nix/haskellPackages.nix {inherit nixpkgs compiler;};

  consumer-data-au-lambdabank = haskellPackages.callPackage ./consumer-data-au-lambdabank.nix {};

  cdrDockerImage = pkgs.dockerTools.buildImage {
    name = "cdr-mock-server";
    tag = "latest";

    contents = [
      consumer-data-au-lambdabank
    ];

    config = {
      Version = "0.1a";
      EntryPoint = ["lambda-bank"];
      ExposedPorts = { "8000/tcp" = {}; };
    };
  };

  drv = cdrDockerImage;
in
  drv
