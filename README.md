# `hlint-plugin`

This is a revival of the [`splint` project](https://github.com/tfausak/splint)
to create an `hlint` plugin for GHC.

The main advantages of doing this are:

- Better integration with GHC tooling

  Now `hlint` errors will appear directly in your tools (e.g. `cabal build`
  `ghcid`, `haskell-language-server`) instead of you having to run `hlint`
  out of band.

- You only have to lint modules that you rebuild

- You don't have to parse the module twice

In order to build this plugin, you will need to:

- build the `hlint` package with the `-f-ghc-lib` cabal configure option
- build the `ghc-lib-parser-ex` package with the `-fno-ghc-lib` cabal configure
  option

To use this plugin, add this package as a build dependency and then enable the
following GHC options (typically in the `ghc-options:` field of your `.cabal`
file):

```bash
-fplugin HLint.Plugin
```

You can pass command-line options to `hlint` using `-fplugin-opt`, like this:

```bash
-fplugin HLint.Plugin -fplugin-opt='HLint.Plugin:--ignore=Redundant guard'
```

## Development

This repository uses Nix for development.  You can build this package entirely
using Nix for a specific version of `ghc` by running:

```ShellSession
$ nix develop .#ghc${MAJOR}${MINOR}
```

… replacing `${MAJOR}` and `${MINOR}` with the major and minor version of the
`ghc` that you're using.  For example, if you're using GHC 9.4, then you'd run:

```ShellSession
$ nix build .#ghc94
```

If you want to develop interactively using Cabal inside of a Nix shell, run:

```ShellSession
$ nix develop .#ghc${MAJOR}${MINOR}
```

Once you are inside that Nix shell, then you can use `cabal` commands, like
`cabal build` or `cabal repl`.  You can also use `ghcid` or launch your favorite
IDE from inside this shell.

To check that this plugin works for all supported GHC versions, run:

```ShellSession
$ nix flake check
```
