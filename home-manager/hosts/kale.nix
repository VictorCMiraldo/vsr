{ pkgs, config, lib, ... }:
{
  vsr.isWorkMachine = false;

  age = {
    identityPaths = [ "${config.home.homeDirectory}/keychain/vsr-secrets/id_ed25519" ];
    secrets = {
      sshPersonalServersData = {
        file = ./secrets/personal-servers-data.age;
        path = "${config.home.homeDirectory}/.ssh/config.d/personal-servers-data.conf";
      };

    };
  };

  home.packages =
    let
      agdaWithStdlib = pkgs.agda.withPackages (p: [ p.standard-library ]);
    in with pkgs; [
        cachix
        konsave
        pandoc
        basedpyright
        agdaWithStdlib
        ripgrep
        nix-tree

        # Formr youtube-dl
        yt-dlp
      ];

}
