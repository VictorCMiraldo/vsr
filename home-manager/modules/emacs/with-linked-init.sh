#! /bin/bash

echo "Linking init and libraries straight to here. Bypassing nix"

mkdir -p "$HOME/.emacs.d/notch"
mkdir -p "$HOME/.emacs.d/modules"

files=$(find -name '*.el')
for f in $files; do
  relative=${f##./}
  echo "Linking: $relative"
  ln -fs "$(pwd)/$relative" "$HOME/.emacs.d/$relative"
done

read -p "Press <Enter> when you're done editing" _finish

for f in $files; do
  relative=${f##./}
  echo "Removing: $relative"
  rm "$HOME/.emacs.d/$relative"
done

echo "You must now invoke home-manager's switch, .emacs.d is empty"
