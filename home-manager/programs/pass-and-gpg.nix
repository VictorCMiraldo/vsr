{config, pkgs, ...}:
{
# Below is a config for bringing pass, gpg and gpg-agent through
# the nix store. Nevertheless, because these are easily available
# throug the default APT source, I'll stick to that and
# just set up the right env variables instead.
  home.sessionVariables = {
    KEYCHAIN_ROOT = "CB4A4FBA";
    PASSWORD_STORE_KEY = "CB4A4FBA";
    PASSWORD_STORE_DIR = "${config.home.homeDirectory}/keychain/password-store";
    GNUPGHOME = "${config.home.homeDirectory}/keychain/gnupg";
  };
}
/*
  home.packages = [
    pkgs.xclip
    pkgs.pinentry-gtk2
  ];

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = {
    };
  };

  # If pass is complaining, its probably because gpg-agent was also installed through
  # apt. Remove it from apt and run `gpgconf --kill all` to force the newer one to run.
  programs.gpg = {
    enable = true;
    homedir = "${config.home.homeDirectory}/keychain/gnupg";
  };
  services.gpg-agent = {
    enable = true;
    extraConfig = ''
      pinentry-program ${pkgs.pinentry-gtk2}/bin/pinentry
    '';
  };
  services.gnome-keyring = {
    enable = true;
    components = [ "secrets" ];
  };
*/
