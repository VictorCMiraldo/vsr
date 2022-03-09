#! /bin/bash
set -euo pipefail

mkdir -p $HOME/.local/share/fonts

for font in $(find $VSR_ROOT/config/fonts -name '*.ttf'); do
  cp $font $HOME/.local/share/fonts/
done

for font in $(find $VSR_ROOT/config/fonts -name '*.otf'); do
  cp $font $HOME/.local/share/fonts/
done

fc-cache -f -v
