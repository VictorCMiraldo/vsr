#! /bin/bash
set -euo pipefail

# Link our nix.conf file and change its settings accordingly
sudo mkdir -p /etc/nix
sudo ln -fs "$(pwd)/nix.conf" /etc/nix/nix.conf
sudo chmod 644 /etc/nix/nix.conf

# Now we install nix-direnv and configure it.
# https://github.com/nix-community/nix-direnv#with-nix-env
nix-env -f '<nixpkgs>' -iA nix-direnv
echo "source $HOME/.nix-profile/share/nix-direnv/direnvrc" > $HOME/.direnvrc
