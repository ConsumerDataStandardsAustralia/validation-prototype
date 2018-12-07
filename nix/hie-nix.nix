{ nixpkgs ? import ./nixpkgs.nix
, ghc843 ? false
, hie
}:
let
  hie-nix-json-path = if ghc843 then ./hie-nix.843.json else ./hie-nix.json;
  hie-nix-src = nixpkgs.pkgs.fetchgit {
    inherit (nixpkgs.pkgs.lib.importJSON hie-nix-json-path) url rev sha256;
  };
  hie-nix = import hie-nix-src {};
in rec {
  hie-tools = if !hie then [] else (with nixpkgs.pkgs.haskellPackages;
    [ Cabal_2_4_0_1 apply-refact hie-nix.hie84 hsimport hasktags hlint hoogle brittany ]);
  addHieTools = drv: nixpkgs.pkgs.haskell.lib.addTool drv hie-tools;
}
