{
  puppet-server =
    { config, pkgs, ... }:
    { deployment.targetHost = "192.168.0.42";
    };
}

