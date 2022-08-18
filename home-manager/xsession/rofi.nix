{pkgs, ...}:
let
  # Save our own trivial version of rofi-pass using rofi "script" mode.
  # On a first call, rofi calls the script without any inputs, hence "$@" is empty.
  # When we press enter, rofi calls the script again and passes the selected entry
  # as input. For more, check the rofi-script man pages.
  my-rofi-pass = pkgs.writeShellScriptBin "rofi-pass" ''
    #! /usr/bin/env bash
    echo -en "\0prompt\x1f\n"
    echo -en "\0no-custom\x1ftrue\n"
    echo -en "\0message\x1fSelect password to copy, ESC to abort\n"
    if [[ ! -z "$@" ]]; then
      coproc (${pkgs.pass}/bin/pass -c "$@")
      exit 0
    else
      find "$PASSWORD_STORE_DIR" -type f \
        | sed "s:$PASSWORD_STORE_DIR/::" | sed "s/\.gpg//"
    fi
  '';
in {
  home.packages = [ 
    # Brings in rofi and the plugins we rely on
    (pkgs.rofi.override { plugins = [ pkgs.rofi-calc ]; }) 
    pkgs.rofi-power-menu 
    my-rofi-pass
  ];

  # Load our rofi configuration
  home.file.".config/rofi/config.rasi".text = import ./rofi-config.nix;
}



