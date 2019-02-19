# How to Contribute

## Code Architecture

There are five main parts of this code that you should familiarise yourself with:
- [Types](./consumer-data-au-types/)
  - [src](./consumer-data-au-types/src): The Specification of all of the URLs, headers and datatypes.
  - [tests](./consumer-data-au-types/tests): Test cases to make sure that the routes and JSON match up to the [upstream spec](https://consumerdatastandardsaustralia.github.io/standards/#introduction). 
- [Lambdabank](./consumer-data-au-lambdabank/)
  - [src](./consumer-data-au-lambdabank/src): A server that implements the types from the spec and returns dummy data.
  - [tests](./consumer-data-au-lambdabank/tests): Test cases to make sure that the server returns the right thing. Uses the client.
- [Client](./consumer-data-au-client/): A client generated from the types project. Can be used to access a standards implementing server.

Note at this point none of this client deals with auth: not even parsing the headers and JWT, but there is work in the types project that has this partially formed (but not wired up).

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
