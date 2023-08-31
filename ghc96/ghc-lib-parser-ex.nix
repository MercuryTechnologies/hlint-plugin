{ mkDerivation, base, bytestring, containers, directory, extra
, filepath, ghc, ghc-boot, ghc-boot-th, lib, tasty, tasty-hunit
, uniplate
}:
mkDerivation {
  pname = "ghc-lib-parser-ex";
  version = "9.6.0.2";
  sha256 = "2a618793031316c46bcbf44ad5f09878a03c52e3c9ad5d45f89d3f48d62826bb";
  configureFlags = [ "-fno-ghc-lib" ];
  libraryHaskellDepends = [
    base bytestring containers ghc ghc-boot ghc-boot-th uniplate
  ];
  testHaskellDepends = [
    base directory extra filepath ghc ghc-boot ghc-boot-th tasty
    tasty-hunit uniplate
  ];
  homepage = "https://github.com/shayne-fletcher/ghc-lib-parser-ex#readme";
  description = "Algorithms on GHC parse trees";
  license = lib.licenses.bsd3;
}
