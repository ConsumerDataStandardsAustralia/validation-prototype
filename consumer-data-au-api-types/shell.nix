{ nixpkgs ? import ../nix/nixpkgs.nix
, compiler ? "default"
, hie ? false
}:

let
  inherit (nixpkgs) pkgs;

  drv = (import ./. {});
  hie-nix = (import ../nix/hie-nix.nix { inherit hie; });

  haskellPackages = import ./nix/haskellPackages.nix {inherit nixpkgs compiler;};

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
    buildTools = (drv'.buildTools or []) ++ hie-nix.hie-tools;
  });

in
  shellDrv.env
