{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, colour, conduit, containers, diagrams
      , diagrams-lib, diagrams-svg, mtl, stdenv, stm, text
      }:
      mkDerivation {
        pname = "lamby2019";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base colour conduit containers diagrams diagrams-lib diagrams-svg
          mtl stm text
        ];
        executableHaskellDepends = [
          base colour conduit containers diagrams diagrams-lib diagrams-svg
          mtl stm text
        ];
        doHaddock = false;
        homepage = "https://github.com/onixie";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
