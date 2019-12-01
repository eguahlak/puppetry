{config, pkgs, lib, ...}:
let
  cfg = config.services.puppetry;

  puppet-master = 
    pkgs.haskellPackages.callPackage ../default.nix {};

  public = ../public;
in
with lib;
 {
   options = {
     services.puppetry = {
       enable = mkOption {
         default = true;
         type = with types; bool;
         description = ''
            Enable the puppetry server
         '';
       };
 
       user = mkOption {
         default = "puppet";
         type = with types; uniq str;
         description = ''
           Name of the user.
         '';
       };
       
       port = mkOption {
         default = "80";
         type = with types; uniq str;
         description = ''
           The port to run on.
         '';
       };
      
       udev = mkOption {
         default = "-";
         type = with types; uniq str;
         description = ''
           Udev.
         '';
       };

     };
   };
 
   config = mkIf cfg.enable {
     systemd.services.puppetrySession = {
       wantedBy = [ "multi-user.target" ]; 
       after = [ "network.target" ];
       description = "Start the puppetry server";
       restartIfChanged = true;
       serviceConfig = {
         Type = "forking";
         User = "${cfg.user}";
         ExecStart = ''${pkgs.screen}/bin/screen -dmS puppet-master ${puppet-master}/bin/puppet-master ${cfg.port} ${cfg.udev} ${public}'';         
         ExecStop = ''${pkgs.screen}/bin/screen -S puppet-master -X quit'';
       };
     };

     users.users.${cfg.user} = {
       isNormalUser = false;
     };

     environment.systemPackages = [ pkgs.screen ];
   };
 }
