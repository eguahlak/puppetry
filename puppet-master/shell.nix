{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, binary, bytestring
      , concurrent-extra, free, hspec, hspec-wai, hspec-wai-json
      , monadplus, mtl, scotty, serialport, stdenv, text, unix, vector
      , wai, wai-middleware-static
      }:
      mkDerivation {
        pname = "puppet-master";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base binary bytestring concurrent-extra free monadplus mtl
          scotty serialport text unix vector wai wai-middleware-static
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
