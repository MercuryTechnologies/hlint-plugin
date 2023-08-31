{ inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/cddebdb60de376c1bdb7a4e6ee3d98355453fe56";

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
    utils.lib.eachSystem [ "ghc90" "ghc92" "ghc94" "ghc96" ] (compiler:
      let
        config = { };

        overlay = self: super: {
          hlint-plugin =
            self.haskell.lib.justStaticExecutables
              self.haskell.packages."${compiler}".hlint-plugin;

          haskell = super.haskell // {
            packages = super.haskell.packages // {
              "${compiler}" = super.haskell.packages."${compiler}".override (old: {
                overrides =
                  self.lib.fold
                    self.lib.composeExtensions
                    (_: _: { })
                    [ (self.haskell.lib.packageSourceOverrides {
                        hlint = {
                          ghc90 = "3.3.6";
                          ghc92 = "3.4.1";
                          ghc94 = "3.5";
                          ghc96 = "3.6.1";
                        }."${compiler}";

                        hlint-plugin = ./.;
                      })

                      (self.haskell.lib.packagesFromDirectory {
                        directory = {
                          ghc90 = ./ghc90;
                          ghc92 = ./ghc92;
                          ghc94 = ./ghc94;
                          ghc96 = ./ghc96;
                        }."${compiler}";
                      })

                      # `ghc-lib-parser-ex` and `hlint` both need to be built
                      # against the `ghc` API and not the `ghc-lib*` APIs,
                      # otherwise the built plugin will not be accepted by GHC.
                      (hself: hsuper: {
                        ghc-lib-parser-ex =
                          self.haskell.lib.enableCabalFlag
                            hsuper.ghc-lib-parser-ex
                            "no-ghc-lib";

                        hlint =
                          self.haskell.lib.disableCabalFlag
                            hsuper.hlint
                            "ghc-lib";
                      })
                    ];
              });
            };
          };
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages.default = pkgs.haskell.packages."${compiler}".hlint-plugin;

          apps.default = {
            type = "app";

            program = "${pkgs.hlint-plugin}/bin/hlint-plugin";
          };

          devShells.default = pkgs.haskell.packages."${compiler}".hlint-plugin.env;

          # This comes in handy when doing dev on the `hlint` package, whose
          # dependencies can be tricky to get right using Nix
          devShells.hlint = pkgs.haskell.packages."${compiler}".hlint.env;
        }
    ));
}
