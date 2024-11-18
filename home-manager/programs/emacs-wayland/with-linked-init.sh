#! /bin/bash

echo "Linking init and libraries straight to here. Bypassing nix"
ln -fs "$(pwd)/init.el" ~/.emacs.d/init.el
mkdir -p "$HOME/.emacs.d/notch" && ln -fs "$(pwd)/notch/notch.el" "$HOME/.emacs.d/notch/notch.el"
mkdir -p "$HOME/.emacs.d/modules"
for f in $(ls -1 "$(pwd)/modules"); do
  echo "Linking $f"
  rm "/home/victor/.emacs.d/modules/$f"
  ln -s "$(pwd)/modules/$f" "$HOME/.emacs.d/modules/$f"
done

read -p "Press <Enter> when you're done editing" _finish
rm ~/.emacs.d/init.el
rm ~/.emacs.d/notch/notch.el

for f in $(ls -1 "$(pwd)/modules"); do
  rm "$HOME/.emacs.d/modules/$f"
done

