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
