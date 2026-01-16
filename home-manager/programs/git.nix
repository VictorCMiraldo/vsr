{config, pkgs, lib, ...}:
{
  config = { 
    programs.git = lib.mkIf (! config.vsr.isWorkMachine) {
      enable = true;
      settings.user = {
        name = "Victor Miraldo";
        email = "victor.miraldo@fastmail.com";
      };
    };
  };
}


