{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
, hie ? false
}:

let
  inherit (nixpkgs) pkgs;

  drv = (import ./. {});
  hie-nix-src = (import ../nix/hie-nix.nix {});
  hie-nix = import hie-nix-src {};

  haskellPackages = import ./nix/haskellPackages.nix {inherit nixpkgs compiler;};
  hie-tools = if !hie then [] else (with pkgs.haskellPackages;
    [ Cabal_2_4_0_1 apply-refact hie-nix.hie84 hsimport hasktags hlint hoogle brittany ]);

  shellDrv = pkgs.haskell.lib.overrideCabal drv (drv': {
    buildDepends =
      (drv'.buildDepends or []) ++
      [ (haskellPackages.hoogleLocal {
          packages =
            (drv'.libraryHaskellDepends or []) ++
            (drv'.executableHaskellDepends or []) ++
            (drv'.testHaskellDepends or []) ;
        })
        pkgs.cabal-install
        haskellPackages.ghcid
      ];
    buildTools = (drv'.buildTools or []) ++ hie-tools;
  });

in
  shellDrv.env
