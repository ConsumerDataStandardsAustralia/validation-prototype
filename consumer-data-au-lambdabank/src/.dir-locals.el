((nil . (
  (dante-target . "consumer-data-au-lambdabank:consumer-data-au-lambdabank")
  (dante-repl-command-line . ("nix-shell" "--run" (concat "cabal new-repl " dante-target " --builddir=dist/dante")))
  (spacemacs/lsp-haskell-nix-shell-args . (list "--args" "hie" "true"))
)))
