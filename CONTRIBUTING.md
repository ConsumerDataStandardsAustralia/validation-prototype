# How to Contribute

## Getting Started

You will need to install [nix](https://nixos.org/nix/) for this project, which means using a nix-supported OS (not Windows, not FreeBSD). Using nix will get you all the right versions of GHC and Cabal for free.

We use the [gitflow](https://danielkummer.github.io/git-flow-cheatsheet/) workflow. There are various supporting toolsets available for performing the high-level gitflow operations (`gitAndTools.gitflow` in nix, https://github.com/nvie/gitflow), if you would prefer to avoid using underlying git commands.

## Making Changes

1. Create a feature branch from `develop`, with the prefix of `feature/`, do some cool, helpful stuff.
1. If you want to solicit feedback feel free to email cdr@qfpl.io.
1. Selectively squash out your WIP commits (`git rebase -i`), but don't feel obliged to squash everything.
1. Merge the latest commits from `develop` into your feature.
1. Create a PR for the branch.
