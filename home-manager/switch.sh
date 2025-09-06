#! /bin/bash

if [[ "$#" -eq "0" ]]; then
  opt="switch"
elif [[ "$1" == "--build" ]]; then
  opt="build"
else
  echo "Only possible option is --build"
  exit 1
fi

# We need to switch --impure on because we read from the system environment 
# to decide whether this is the work machine or not
home-manager $opt --flake .#default --impure --show-trace
