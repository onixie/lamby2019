{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./lamby2019.nix {});

in

  with pkgs; lib.overrideDerivation (if lib.inNixShell then drv.env else drv) (old: {

    buildInputs = old.buildInputs ++ [
      mesa_drivers
    ];

    LIBGL_DRIVERS_PATH="${mesa_drivers}/lib/dri";
    LD_LIBRARY_PATH="${mesa_drivers}/lib:\$LD_LIBRARY_PATH";
  })
