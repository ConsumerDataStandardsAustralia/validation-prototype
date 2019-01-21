# How to Contribute

## Getting Started

You will need to install [nix](https://nixos.org/nix/) for this project, which means using a nix-supported OS (not Windows, not FreeBSD). Using nix will get you all the right versions of GHC and Cabal for free.

We use the [gitflow](https://danielkummer.github.io/git-flow-cheatsheet/) workflow. There are various supporting toolsets available for performing the high-level gitflow operations, if you would prefer to avoid using underlying git commands (`gitAndTools.gitflow` in nix, https://github.com/nvie/gitflow).

## Building, Developing, Testing

* To build the project (including tests and docs), `cd` into one of the packages (client, types, lambdabank) and execute `nix-build` from a terminal.
* To obtain a development environment loaded with all project dependencies, run `nix-shell` inside one of the package directories.
* To run tests, execute `./scripts/test-loop` from within one of the package directories.

## Making Changes

1. Create a feature branch from `develop`, with the prefix of `feature/`, do some cool, helpful stuff.
1. If you want to solicit feedback feel free to email cdr@qfpl.io.
1. Selectively squash out your WIP commits (`git rebase -i`), but don't feel obliged to squash everything.
1. Merge the latest commits from `develop` into your feature.
1. Create a PR for the branch.

## Code Style

We don't have a comprehensive style guide, however there are a few things we try to stick to:

- Keep line lengths to <= 80 columns.
- Use explicit or qualified imports.
- Format imports with `stylish-haskell`.
