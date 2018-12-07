{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, hie ? false
}:
let
  #overrides = import ./nix/consumer-data-au-mock-consumer-overrides.nix;
  wsrc = pkgs: pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "waargonaut";
      rev = "d9759df1b1eaced87efab82bb9f92d7ee3c14024";
      sha256 = "1d1lzyz36w4p0x6k538fgzfmf9fzwgcxldk8xqr2slkghgk778vn";
  };
  w-deps = pkgs: (import "${wsrc pkgs}/waargonaut-deps.nix") pkgs;
  hie-nix = (import ../nix/hie-nix.nix { ghc843 = true; inherit hie; });
in
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  withHoogle = true;
  tools = _: hie-nix.hie-tools;
  overrides = self: super: (w-deps pkgs self super) // {
    waargonaut = pkgs.haskell.lib.dontCheck (self.callPackage (import "${wsrc pkgs}/waargonaut.nix") {});
    # Tries to call stdlib.h calloc. That's not JS, silly!
    base-compat-batteries = pkgs.haskell.lib.dontCheck super.base-compat-batteries;
    # /capture/captures test fails with an exception: *** Exception: JavaScript exception: h$base_dup
    silently = pkgs.haskell.lib.dontCheck super.silently;
    # Tests fail trying to call a c function called mkdir
    mockery = pkgs.haskell.lib.dontCheck super.mockery;
    # Test fail: ReferenceError: h$unliftio_inittime is not defined
    unliftio = pkgs.haskell.lib.dontCheck super.unliftio;
    # ReferenceError: h$geteuid is not defined
    hw-hspec-hedgehog = pkgs.haskell.lib.dontCheck super.hw-hspec-hedgehog;
    # ReferenceError: h$geteuid is not defined
    bits-extra = pkgs.haskell.lib.dontCheck super.bits-extra;
    # h$geteuid is not defined
    hw-prim = pkgs.haskell.lib.dontCheck super.hw-prim;
    # h$geteuid is not defined
    hw-rankselect-base = pkgs.haskell.lib.dontCheck super.hw-rankselect-base;
    # h$geteuid is not defined
    hw-rankselect = pkgs.haskell.lib.dontCheck super.hw-rankselect;
    # Test fail: ReferenceError: h$opendir is not defined
    conduit = pkgs.haskell.lib.dontCheck super.conduit;
    # Hpack wont work because it relies transitively on libyaml. But some packages need it.
    # Steal the ghc one
    hpack = pkgs.haskellPackages.hpack;

    # These tests depend on regex-posix via test-framework-th -> language-haskell-extract
    monad-par = pkgs.haskell.lib.dontCheck super.monad-par;
    Glob = pkgs.haskell.lib.dontCheck super.Glob;

    # Stuff that has doctests.
    zippers = pkgs.haskell.lib.dontCheck super.zippers;
  };
})
