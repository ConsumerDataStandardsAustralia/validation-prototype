{ nixpkgs ? import ../../nixpkgs.nix }:
nixpkgs.pkgs.fetchgit {
  inherit (nixpkgs.pkgs.lib.importJSON ./jose.json) url rev sha256;
}
