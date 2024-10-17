#! /bin/bash

echo "Linking ~/.emacs.d/init.el directly to this init; bypassing nix."
ln -fs "$(pwd)/init.el" ~/.emacs.d/init.el

read -p "Press <Enter> when you're done editing" _finish
rm ~/.emacs.d/init.el
