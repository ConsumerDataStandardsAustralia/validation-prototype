{ supportedSystems ? ["x86_64-linux"]
, supportedCompilers ? [ "ghc844" ]
, packageName
}:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });

let
  pkgs = import <nixpkgs> {};

  configurations =
    pkgs.lib.listToAttrs (
      pkgs.lib.concatMap (compiler:
        pkgs.lib.concatMap (system:
          [{name = packageName + ":" + compiler + ":" + system; value = {inherit compiler system;};}]
        ) supportedSystems
      ) supportedCompilers
    );

  jobs =
      pkgs.lib.mapAttrs (name: configuration:
          let
            compiler = configuration.compiler;
            system = configuration.system;
            nixpkgs = { pkgs = pkgsFor system; };
          in
            import (../. + "/${packageName}/default.nix") { inherit nixpkgs compiler; }
      ) configurations;
in
jobs
