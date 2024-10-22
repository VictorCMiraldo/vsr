#! /bin/bash

echo "Linking init and libraries straight to here. Bypassing nix"
ln -fs "$(pwd)/init.el" ~/.emacs.d/init.el
mkdir -p "~/.emacs.d/notch" && ln -fs "$(pwd)/notch/notch.el" ~/.emacs.d/notch/notch.el

read -p "Press <Enter> when you're done editing" _finish
rm ~/.emacs.d/init.el
rm ~/.emacs.d/notch/notch.el
