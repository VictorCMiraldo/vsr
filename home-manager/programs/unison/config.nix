{pkgs, ...}:
{
  home.packages = [
    pkgs.unison
  ];

  home.file.".unison/common".source = ./common;
  home.file.".unison/doc.prf".source = ./doc.prf;
  home.file.".unison/keychain.prf".source = ./keychain.prf;
}
