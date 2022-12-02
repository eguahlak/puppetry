{ pkgs ? import <nixpkgs> { }
, compiler ? "default"
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";
in
haskellPackages.developPackage {
  root = builtins.filterSource
    (path: type: baseNameOf path != ".nix")
    ./.;
  name = "puppetry";
  source-overrides = {
    serialport =
      builtins.fetchTarball {
        url = "https://hackage.haskell.org/package/serialport-0.5.1/serialport-0.5.1.tar.gz";
        sha256 = "1dx5gz48kal805sl47kh8vd83gs55knqx1incm7b2xdpsyg1sb0a";
      };
  };
  overrides = hsuper: hself: {
    serialport = pkgs.haskell.lib.dontCheck hself.serialport;
  };
  modifier = drv:
    with pkgs.haskell.lib;
    addBuildTools drv (with haskellPackages; [ cabal-install ghcid hpack ])
  ;
}
