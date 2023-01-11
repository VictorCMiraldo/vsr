{config, pkgs, lib, ...}:
{
  config = { 
    programs.git = lib.mkIf (! config.vsr.isWorkMachine) {
      enable = true;
      userName = "Victor Miraldo";
      userEmail = "victor.miraldo@fastmail.com";
    };
  };
}


