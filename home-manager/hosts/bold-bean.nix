{ config, pkgs, ... }:
{
  vsr.isWorkMachine = true;
  age = {
    identityPaths = [ "${config.home.homeDirectory}/keychain/vsr-secrets/id_ed25519" ];
    secrets = {
      sshWorkServersData = {
        file = ./secrets/work-servers-data.age;
        path = "${config.home.homeDirectory}/.ssh/extra/work-servers-data.conf";
      };
    };
  };

  home.packages =
    with pkgs; [
        cachix
        konsave
        pandoc
        basedpyright
        agdaWithStdlib
        ripgrep
        nix-tree
      ];
}
