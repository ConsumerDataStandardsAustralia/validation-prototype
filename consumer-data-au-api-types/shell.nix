{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
, hie ? false
}:

let
  inherit (nixpkgs) pkgs;

  drv = (import ./. {});

  haskellPackages = import ./nix/haskellPackages.nix {inherit nixpkgs compiler;};
  hie-tools = if !hie then [] else (with pkgs.haskellPackages;
    [ Cabal_2_4_0_1 apply-refact hsimport hasktags hlint hoogle brittany ]);

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
      ];
    buildTools = (drv'.buildTools or []) ++ hie-tools;
  });

in
  shellDrv.env
