{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, binary, bytestring, free, hspec
      , hspec-wai, hspec-wai-json, scotty, serialport, stdenv, wai
      , wai-middleware-static
      }:
      mkDerivation {
        pname = "puppet-master";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base binary bytestring free scotty serialport wai
          wai-middleware-static
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ aeson base hspec hspec-wai hspec-wai-json ];
        homepage = "https://github.com/githubuser/puppet-master#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
