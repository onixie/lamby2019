{ mkDerivation, base, colour, conduit, containers, diagrams
, diagrams-lib, diagrams-rasterific, diagrams-svg, gloss, lens, mtl
, random, repa, stdenv, stm, text
}:
mkDerivation {
  pname = "lamby2019";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base colour conduit containers diagrams diagrams-lib
    diagrams-rasterific diagrams-svg gloss lens mtl random repa stm
    text
  ];
  executableHaskellDepends = [
    base colour conduit containers diagrams diagrams-lib
    diagrams-rasterific diagrams-svg gloss lens mtl random repa stm
    text
  ];
  doHaddock = false;
  homepage = "https://github.com/onixie";
  license = stdenv.lib.licenses.bsd3;
}
