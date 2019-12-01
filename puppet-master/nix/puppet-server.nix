{
  network.description = "Puppet Server";
  
  puppet-server = 
  { config, lib, ...}: 
  { 
    imports = [ ./puppetry.nix ];

    services.puppetry = { 
      enable = true;
      user = "puppet";
    };

    networking.firewall.allowedTCPPorts = [ 80 ];
  };
}

