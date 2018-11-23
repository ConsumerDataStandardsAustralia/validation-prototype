{ nixpkgs, declInput }:

let
  pkgs = import nixpkgs {};
  mkJobJson = packageName: ''
    "${packageName}": {
      "enabled": 1,
      "hidden": false,
      "description": "${packageName}",
      "nixexprinput": "conformance-develop",
      "nixexprpath": "${packageName}/ci.nix",
      "checkinterval": 300,
      "schedulingshares": 1,
      "enableemail": false,
      "emailoverride": "",
      "keepnr": 5,
      "inputs": {
          "conformance-develop": {
            "type": "git",
            "value": "https://github.com/ConsumerDataStandardsAustralia/conformance feature/top-level-ci",
            "emailresponsible": false
          },
          "nixpkgs": {
            "type": "git",
            "value": "https://github.com/NixOS/nixpkgs.git 80738ed9dc0ce48d7796baed5364eef8072c794d",
            "emailresponsible": false
          }
      }
    }'';

in
{
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
      ${mkJobJson "consumer-data-au-api-client"},
      ${mkJobJson "consumer-data-au-api-types"},
      ${mkJobJson "consumer-data-au-lambdabank"}
    }
    EOF
  '';
}
