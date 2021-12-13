{ compiler ? "default"
, pkgs ? import <nixpkgs> {}
, pkgsLinux ? import <nixpkgs> { system = "x86_64-linux"; }
, overrides ? {}
}@input:
let
  v1 = import ./default.nix { compiler = "default"; pkgs = pkgsLinux; } ;
  smallpkgs = pkgs.haskell.lib.justStaticExecutables v1 ;
  my-public = ./public;
in pkgs.dockerTools.buildLayeredImage {
  name = "kalhauge/puppet-master";
  tag = "latest";
  contents = [ smallpkgs ];
  extraCommands = ''
    cp -r ${my-public} public
  '';
  config = {
    # Env = [ "LANG=\"en_US.UTF-8\"" "LC_ALL=\"en_US.UTF-8\"" ];
    Cmd = [ "${smallpkgs}/bin/puppet-master" ];
  };

}
