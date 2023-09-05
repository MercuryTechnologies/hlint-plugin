{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";

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
                          # The following line is not actually used because the
                          # Nixpkgs version we're on doesn't include
                          # `hlint-3.6.1`, yet.  Instead, the actual
                          # `hlint-3.6.1` build comes from `ghc96/hlint.nix`.
                          # Once we upgrade to a new enough Nixpkgs we can
                          # delete that file and then the following line will
                          # kick in and work.
                          ghc96 = "3.6.1";
                        }."${compiler}";

                        hlint-plugin =
                          self.lib.cleanSourceWith
                            { filter = name: type:
                                    self.lib.cleanSourceFilter name type
                                &&  !(self.lib.hasSuffix ".nix" name)
                                &&  !(builtins.baseNameOf name == "README.md");

                              src = ./.;
                            };
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
          packages = pkgs.haskell.packages."${compiler}".hlint-plugin;

          apps = {
            type = "app";

            program = "${pkgs.hlint-plugin}/bin/hlint-plugin";
          };

         checks =
           let
             module = pkgs.writeText "Main.hs"
               ''
               {-# LANGUAGE OverloadedStrings #-}

               main :: IO ()
               main | otherwise = (mempty)
               '';

             expected = pkgs.writeText "expected.txt"
               ''

               Main.hs:4:20: warning:
                   Redundant bracket
                   Perhaps: mempty
                 |
               4 | main | otherwise = (mempty)
                 |                    ^^^^^^^^
               '';
           in
             pkgs.runCommand "hlint-plugin-${compiler}-test"
               { nativeBuildInputs = [
                   (pkgs.haskell.packages."${compiler}".ghcWithPackages
                     (pkgs: [ pkgs.hlint-plugin ])
                   )
                 ];
               }
               ''
               ln --symbolic ${module} Main.hs
               ghc -fplugin HLint.Plugin -fplugin-opt='HLint.Plugin:--ignore=Redundant guard' -Wno-missed-extra-shared-lib -c Main.hs 2> >(tee actual.txt >&2)
               diff ${expected} actual.txt
               touch $out
               '';

          devShells = pkgs.haskell.packages."${compiler}".hlint-plugin.env;
        }
    ));
}
