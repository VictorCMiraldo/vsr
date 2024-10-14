{pkgs, ...}: {
  home.file.".local/bin/my-wofi-pass" = {
    executable = true;
    text = ''
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

          printf "%s" "$(printf '%s\n' "''${password_files}" | wofi --dmenu)"
      }

      function main() {
          local passname
          local field

          passname="$(get_passname_from_menu)"
          [ -n "''${passname}" ] || exit

          pass_get "''${passname}" | wl-copy
          if [ "$?" -eq 0 ]; then
            notify-send "Copied ''${passname}"
          else
            notify-send "Failed"
          fi
      }

      main "''${@}"
    '';
  };
}
