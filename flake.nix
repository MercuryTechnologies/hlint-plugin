{ inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/cddebdb60de376c1bdb7a4e6ee3d98355453fe56";

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    let
      compiler = "ghc96";

    in
      utils.lib.eachDefaultSystem (system:
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
                          hlint = "3.6.1";
                          hlint-plugin = ./.;
                        })
                        (self.haskell.lib.packagesFromDirectory {
                          directory = ./nix;
                        })
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
            devShells.hlint = pkgs.haskell.packages."${compiler}".hlint.env;
          }
      );
}
