{pkgs, ...}:
{
  home.packages = [ pkgs.xclip ];

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = {
      PASSWORD_STORE_DIR = "$HOME/keychain/password-store";
      PASSWORD_STORE_KEY = "CB4A4FBA";
    };
  };
}
