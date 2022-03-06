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

# Download and install nix in single-user mode; multi user is giving me way too much trouble.
sh <(curl -L https://nixos.org/nix/install) --no-daemon

