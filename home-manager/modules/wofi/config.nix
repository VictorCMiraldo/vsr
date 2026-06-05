{
  config,
  pkgs,
  lib,
  ...
}:
let
  wofi-pass = pkgs.writeShellScriptBin "wofi-pass" ''
    #!/usr/bin/env bash

    # This is my own adaptation of the amazing https://github.com/schmidtandreas/wofi-pass
    # I can't use wofi-pass directly since it assumes a little too much structore on my
    # password directory.

    set -o pipefail

    function pass_get() {
        local -r passname="''${1}"

        if [ "$(basename $passname)" == "2fa" ]; then
            pass otp "''${passname}" | tail -n1 | { IFS= read -r pass; printf %s "''${pass}"; }
        else
            pass show "''${passname}" | { IFS= read -r pass; printf %s "''${pass}"; }
        fi
    }

    function get_passname_from_menu() {
        local -r pass_dir="''${PASSWORD_STORE_DIR}"
        local password_files
        password_files="$(find "''${pass_dir}" -name "*.gpg" | sed "s|^''${pass_dir}\/\(.*\)\.gpg$|\1|" | sort)"
        readonly password_files

        printf "%s" "$(printf '%s\n' "''${password_files}" | ${pkgs.wofi}/bin/wofi -i -M multi-contains --dmenu)"
    }

    function main() {
        local passname
        local tout=15

        passname="$(get_passname_from_menu)"
        [ -n "''${passname}" ] || exit

        pass_get "''${passname}" | ${pkgs.wl-clipboard}/bin/wl-copy --sensitive
        if [ "$?" -eq 0 ]; then
          notify-send "Copied ''${passname}"
        else
          notify-send "Failed"
        fi


    }

    main "''${@}"
  '';
in
{
  config = lib.mkIf (!config.vsr.isServer) {
    home.packages = [ wofi-pass ];
    programs.wofi.enable = true;
    programs.wofi.style = with config.lib.stylix.colors; ''
      * {
        font-family: "Hack Nerd Font Mono", monospace;
      }

      window {
        background-color: #${base01};
      }

      #input {
        margin: 5px;
        border-radius: 0px;
        border: none;
        background-color: #${base01};
        color: #${base05};
      }

      #inner-box {
        background-color: #${base00};
      }

      #outer-box {
        margin: 2px;
        padding: 10px;
        background-color: #${base00};
      }

      #scroll {
        margin: 5px;
      }

      #text {
        padding: 4px;
        color: #${base05};
      }

      #entry:nth-child(even) {
        background-color: #${base02};
      }

      #entry:selected {
        background-color: #${base03};
      }

      #text:selected {
        background: transparent;
      }
    '';
  };
}
