#! /bin/bash
wd="${BASH_SOURCE%/*}"
vsr_root="${wd}/../.."
source "$vsr_root/prelude"

set -euo pipefail

# Download and install nix in single-user mode.
# multi user is always gives me way too much trouble.
sh <(curl -L https://nixos.org/nix/install) --no-daemon

# Link our nix.conf file and change its settings accordingly
sudo mkdir -p /etc/nix
sudo ln -fs $(readlink -f "${wd}/nix.conf") /etc/nix/nix.conf
sudo chmod 644 /etc/nix/nix.conf

# Now, we need to source the necessary nix env
export NIX_PATH=$HOME/.nix-defexpr/channels
. $HOME/.nix-profile/etc/profile.d/nix.sh

# Fix nix-channels:
nix-channel --add https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz home-manager
nix-channel --add https://nixos.org/channels/nixos-22.05 nixpkgs
nix-channel --update

# Now we install home-manager and we're golden!
nix-shell '<home-manager>' -A install
