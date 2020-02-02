# ghup

Simple Github helper for myself.

## Installation

If you don't have haskell GHC and cabal installed,
follow [ghcup](https://www.haskell.org/ghcup/) first.

Then issue:

```sh
$ cabal v2-install ghup
```

## Usage

First you need to set the github OAuth (no other method currently supported)
for API access:

```
ghup config --oauth "<your-github-token>"
```

Then follow the help page:

```
Usage: ghup COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  fork                     Fork a repository
  config                   Set ghup config (such as OAuth)
  delete                   Delete a forked repository
  list-forks               List my forks
  gistc                    Create gist
  gistl                    List gists
```
