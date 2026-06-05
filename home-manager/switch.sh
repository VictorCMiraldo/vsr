#! /bin/bash

if [[ "$#" -eq "0" ]]; then
  opt="switch"
elif [[ "$1" == "--build" ]]; then
  opt="build"
else
  echo "Only possible option is --build"
  exit 1
fi

# We use flakes, so no need for channels anywhere
export NIX_PATH=""

home-manager $opt --flake .#$(hostname) --show-trace
