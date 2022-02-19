#! /bin/bash
set -euo pipefail

vsr_root="${BASH_SOURCE%/*}/../.."
source "$vsr_root/prelude"

# Make sure we can sudo
sudo -k
errcho "I need sudo."
if ! sudo true; then
  errcho "Sorry, aborting."
  exit 1
fi

# Download and install nix in multi-user mode:
sh <(curl -L https://nixos.org/nix/install) --daemon

# Link our nix.conf file and change its settings accordingly
sudo ln -fs "$(pwd)/nix.conf" /etc/nix/nix.conf
sudo chmod 644 /etc/nix/nix.conf

# Finally, we must restart the nix-daemon for changes to take effect.
sudo systemctl restart nix-daemon

# Add a nixpkgs channel
echo "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz nixpkgs" > ~/.nix-channels
nix-channel --update

# Now we install nix-direnv and configure it.
# https://github.com/nix-community/nix-direnv#with-nix-env
nix-env -f '<nixpkgs>' -iA nix-direnv
echo "source $HOME/.nix-profile/share/nix-direnv/direnvrc" > $HOME/.direnvrc
