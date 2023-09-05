{ mkDerivation, aeson, ansi-terminal, base, bytestring, cmdargs
, containers, cpphs, data-default, deriving-aeson, directory, extra
, file-embed, filepath, filepattern, ghc-lib-parser
, ghc-lib-parser-ex, hscolour, lib, process, refact, text
, transformers, uniplate, unordered-containers, utf8-string, vector
, yaml
}:
mkDerivation {
  pname = "hlint";
  version = "3.6.1";
  sha256 = "3280132bb3c1b39faa4c8c6a937479d4622484914da6a227a2ce4b15a76741fd";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring cmdargs containers cpphs
    data-default deriving-aeson directory extra file-embed filepath
    filepattern ghc-lib-parser ghc-lib-parser-ex hscolour process
    refact text transformers uniplate unordered-containers utf8-string
    vector yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/ndmitchell/hlint#readme";
  description = "Source code suggestions";
  license = lib.licenses.bsd3;
  mainProgram = "hlint";
}
