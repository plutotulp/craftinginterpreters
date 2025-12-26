{ mkDerivation, base, lib }:
mkDerivation {
  pname = "hlox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "hlox";
}
