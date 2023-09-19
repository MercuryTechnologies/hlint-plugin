1.0.2

- [Fix performance of `argsSettings`](https://github.com/MercuryTechnologies/hlint-plugin/pull/13)
  - Before this change `hlint-plugin` would read in the hlint settings file for
    each Haskell module that was compiled and now it correctly caches those
    reads, which greatly improves the performance of the plugin.
- Small improvements to package description

1.0.1

- Small improvements to package description

1.0.0

- Initial release
