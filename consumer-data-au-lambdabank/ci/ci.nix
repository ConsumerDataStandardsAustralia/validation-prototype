{ supportedSystems ? ["x86_64-linux"]
, supportedCompilers ? [ "ghc844" ]
}:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });

let
  pkgs = import <nixpkgs> {};

  configurations =
    pkgs.lib.listToAttrs (
      pkgs.lib.concatMap (compiler:
        pkgs.lib.concatMap (system:
          [{name = "haskell.packages." + compiler + ".consumer-data-au-lambdabank." + system ; value = {inherit compiler system;};}]
        ) supportedSystems
      ) supportedCompilers
    );

  jobs =
      pkgs.lib.mapAttrs (name: configuration:
          let
            compiler = configuration.compiler;
            system = configuration.system;
            nixpkgs = { pkgs = pkgsFor system; };
            consumer-data-au-lambdabank = import ../default.nix { inherit nixpkgs compiler; };
          in
            consumer-data-au-lambdabank
      ) configurations;
in
jobs
