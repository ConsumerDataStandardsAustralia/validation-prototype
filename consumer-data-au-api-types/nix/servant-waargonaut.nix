{ nixpkgs ? import ../../nixpkgs.nix }:
nixpkgs.pkgs.fetchgit {
  inherit (nixpkgs.pkgs.lib.importJSON ./servant-waargonaut.json) url rev sha256;
}
