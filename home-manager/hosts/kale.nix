{
  pkgs,
  config,
  lib,
  ...
}:
{
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "25.05";

  vsr.isWorkMachine = false;
  vsr.isServer = false;

  age = {
    identityPaths = [ "${config.home.homeDirectory}/keychain/vsr-secrets/id_ed25519" ];
    secrets = {
      sshPersonalServersData = {
        file = ../secrets/personal-servers-data.age;
        path = "${config.home.homeDirectory}/.ssh/config.d/personal-servers-data.conf";
      };

    };
  };

  home.packages =
    let
      agdaWithStdlib = pkgs.agda.withPackages (p: [ p.standard-library ]);
    in
    with pkgs;
    [
      cachix
      konsave
      pandoc
      basedpyright
      agdaWithStdlib
      ripgrep
      nix-tree

      # Former youtube-dl
      yt-dlp

      # Great at detecting duplicate files
      rmlint
    ];

}
