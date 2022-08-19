{config, pkgs, ...}:
{
  home.packages = [
    pkgs.pinentry-gtk2 
  ];

  # If pass is complaining, its probably because gpg-agent was also installed through
  # apt. Remove it from apt and run `gpgconf --kill all` to force the newer one to run.
  programs.gpg = {
    enable = true;
    homedir = "${config.home.homeDirectory}/keychain/gnupg";
  };
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry-gtk2}/bin/pinentry
    '';
  };
}

