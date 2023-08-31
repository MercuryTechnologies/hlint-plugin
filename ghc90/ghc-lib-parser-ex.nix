{ mkDerivation, base, bytestring, containers, directory, extra
, filepath, ghc-lib-parser, lib, tasty, tasty-hunit, uniplate
}:
mkDerivation {
  pname = "ghc-lib-parser-ex";
  version = "9.0.0.6";
  sha256 = "87d51652af8f71c0f3e6074c2ad5eb124ea26b0cde8a800f204d704d7a6b10d3";
  libraryHaskellDepends = [
    base bytestring containers ghc-lib-parser uniplate
  ];
  testHaskellDepends = [
    base directory extra filepath ghc-lib-parser tasty tasty-hunit
    uniplate
  ];
  homepage = "https://github.com/shayne-fletcher/ghc-lib-parser-ex#readme";
  description = "Algorithms on GHC parse trees";
  license = lib.licenses.bsd3;
}
