{ pkgs ? import <nixpkgs> {} }:
let 
  puppet-master = pkgs.haskellPackages.callPackage ./haskell {};
  puppet-client = import ./elm {};
in
  pkgs.stdenv.mkDerivation {
    name = "puppet-master";
    buildInputs = [ puppet-master ];
  }
