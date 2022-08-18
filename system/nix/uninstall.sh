#! /bin/bash
set -euo pipefail

echo "Will uninstall nix. You will have to rebuild all derivations. Are you sure?"

read -p "Are you sure (type: yes)? " answer

if [[ "$answer" == "yes" ]]; then
  sudo rm -rfv /etc/nix /nix /root/.nix-profile /root/.nix-defexpr /root/.nix-channels /home/victor/.nix-profile /home/victor/.nix-defexpr /home/victor/.nix-channels
else
  echo "Aborting"
fi
