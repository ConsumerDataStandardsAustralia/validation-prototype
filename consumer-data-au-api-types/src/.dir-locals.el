((nil . (
  (dante-target . "consumer-data-au-api-types:consumer-data-au-api-types")
  (dante-repl-command-line . ("nix-shell" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/dante")))
  (spacemacs/lsp-haskell-nix-shell-args . (list "--args" "hie" "true"))
)))
