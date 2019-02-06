with import <nixpkgs> {};

let
  cdr-mock-server = import ./default.nix;
in {
  sandboxImage = dockerTools.buildImage {
    name = "cdr-sandbox";
    tag = "latest";

    contents = [
      cdr-mock-server
    ];

    config = {
      Version = "0.1a";
      EntryPoint = ["cdr-mock-server"];
#      ExposedPorts = { "433/tcp" = {}; };
    };
  };
}
