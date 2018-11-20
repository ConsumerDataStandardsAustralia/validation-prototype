#! /usr/bin/env nix-shell
#! nix-shell -p nix-prefetch-git -i bash
HERE=$(dirname $0)
nix-prefetch-git https://github.com/qfpl/servant-waargonaut $1 > $HERE/servant-waargonaut.json
