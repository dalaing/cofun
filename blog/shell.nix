{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hakyll, stdenv }:
      mkDerivation {
        pname = "cofun-blog";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ base hakyll ];
        librarySystemDepends = [ nixpkgs.darwin.apple_sdk.frameworks.Cocoa ];
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
