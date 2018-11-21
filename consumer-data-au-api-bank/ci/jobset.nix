{ nixpkgs, declInput }: let pkgs = import nixpkgs {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "consumer-data-au-api-bank": {
            "enabled": 1,
            "hidden": false,
            "description": "consumer-data-au-api-bank",
            "nixexprinput": "conformance-develop",
            "nixexprpath": "consumer-data-au-api-bank/ci/ci.nix",
            "checkinterval": 300,
            "schedulingshares": 1,
            "enableemail": false,
            "emailoverride": "",
            "keepnr": 5,
            "inputs": {
                "conformance-develop": {
                  "type": "git",
                  "value": "https://github.com/ConsumerDataStandardsAustralia/conformance develop",
                  "emailresponsible": false
                },
                "nixpkgs": {
                  "type": "git",
                  "value": "https://github.com/NixOS/nixpkgs.git 80738ed9dc0ce48d7796baed5364eef8072c794d",
                  "emailresponsible": false
                }
            }
        }
    }
    EOF
  '';
}
