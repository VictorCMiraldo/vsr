#! /bin/bash
wd="${BASH_SOURCE%/*}"
source "${wd}/prelude"

set -euo pipefail

# Install the system-level config:
# - nix + home-manager
# - gtk2/3 themes
# - apt packages to complement my home-manager config
${wd}/system/install-system.sh

## Setup our base directory structure
mydirs=("$HOME/tmp/Desktop"
        "$HOME/data/Music"
        "$HOME/data/Videos"
        "$HOME/data/Pictures"
        "$HOME/keychain"
        "$HOME/repos"
        "$HOME/doc"
        "$HOME/bin")
 
for d in ${mydirs[*]}; do
   if [[ ! -d "$d" ]]; then
       mkdir -p "$d"
   fi
done

rmdirs=("$HOME/Music"
        "$HOME/Documents"
        "$HOME/Downloads"
        "$HOME/Public"
        "$HOME/Templates"
        "$HOME/Pictures"
        "$HOME/Desktop"
        "$HOME/Videos")

for d in ${rmdirs[*]}; do
  if [[ -d "$d" ]]; then
    rmdir "$d"
  fi
done

## Legagy Config are those aspects of my config that
## were not ported to home-manager still 

## Link our unison profiles; we create a dedicated directory because unison loves
## to keep some of its own local control files in there, so better not pollute
## vsr with it.
mkdir -p "$HOME/.unison" \
  && ln -fs $(readlink -f "${wd}/legacy-config/unison")/* "$HOME/.unison/"

## Link our templates
ln -fs $(readlink -f "${wd}/legacy-config/templates") "$HOME/.templates"

## Link our papis config
mkdir -p "$HOME/.config"
ln -fs $(readlink -f "${wd}/legacy-config/papis") "$HOME/.config/papis"

## Finishing up


# Link our scripts folder
if [[ ! -e "$HOME/.bin" ]]; then
  ln -fs $(readlink -f "${wd}/scripts") "$HOME/.bin"
fi

# This file should be ran after `install.sh` has finished. Here we remove the
# files that will conflict with home-manager, then switch to using it.
rm -f "$HOME/.config/user-dirs.dirs"
rm -f "$HOME/.profile"
rm -f "$HOME/.bashrc"

# Link our home-manager config
rm -rf "$HOME/.config/nixpkgs"
ln -fs $(readlink -f "${wd}/home-manager") "$HOME/.config/nixpkgs"

# Now we actually run the home-manager and switch everything
. $HOME/.nix-profile/etc/profile.d/nix.sh
$HOME/.nix-profile/bin/home-manager switch

# And finish off installing xmonad
~/.xmonad/install.sh

echo "All good! Please log off then on again"
