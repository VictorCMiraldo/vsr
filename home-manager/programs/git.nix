{config, pkgs, lib, inputs, ...}:
{
  config = { 
    programs.git = lib.mkIf (! inputs.isWorkMachine) {
      enable = true;
      userName = "Victor Miraldo";
      userEmail = "victor.miraldo@fastmail.com";
    };
  };
}


