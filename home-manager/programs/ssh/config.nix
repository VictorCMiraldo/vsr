{config, pkgs, lib, isWorkMachine, ...}:
{
  config = { 
    programs.ssh = lib.mkIf (! isWorkMachine) {
      enable = true;
      matchBlocks = {
        "pizero" = {
          hostname = "192.168.2.1";
          user = "pi";
          identityFile = "/home/victor/keychain/vps/PiZero/id_rsa";
        };
      };
    };
  home.file.".ssh/authorized_keys".text = ''
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHSwnBiaBmNkqUJyRfOjnE0rOwb80Bgkko149d/R/pXW victor@beetroot
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO40gphiJ99S0qmbYqLagjuUf9+x7+6khz5CvZe2MpjO victor@blackbean
    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOI2iuzx7gQAuxi03FGgo4hrCy897X4zUjBV7Vtg+1Ue victor@dev-lt-60
    '';
  };
}


