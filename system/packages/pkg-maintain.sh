#! /bin/bash

wd="${BASH_SOURCE%/*}"
source "${wd}/../../prelude"
set -euo pipefail

function showHelp() {
  errcho "usage: pkg-maintain [opts]"
  errcho ""
  errcho "  pkg-maintain will maintain the packages on this system."
  errcho "  We shall read the list of packages to install and remove"
  errcho "  from config/packages."
  errcho ""
  errcho "  -v  --verbose   chatty output on stderr."
  errcho "  -h  --help      this screen."
  errcho "  -d  --dry-run   makes a dry-run (don't install or remove anything)."
  errcho "                  implies -v"
  errcho "  -u  --update    Update apt before doing changes."
}

verb=false
dryrun=false
update=false
while [[ $# -gt 0 ]]; do
  key=$1
  shift
  case $key in
    -v|--verbose)
      verb=true
    ;;
    
    -u|--update)
      update=true
    ;;

    -d|--dry-run)
      dryrun=true
    ;;

    *)
      showHelp
      exit 1
    ;;
  esac
done

# Makes sure we have the files we need!
uninstalls="${wd}/uninstall"
installs="${wd}/install"
if [[ ! -e $uninstalls ]] || [[ ! -e $installs ]]; then
  errcho "Package configuration files are missing."
  errcho "Aborting"
  return 1
fi 

if $update; then
  sudo apt-get -qq update
fi

# Handle uninstalls
pkgs=""
while read line; do
  if pkg-is-installed "$line"; then
    errcho "Scheduling removal: $line"
    pkgs="${pkgs} $line"
  else
    errcho "Package is already gone: $line"
  fi
done < <(cat "$uninstalls" | uncomment)
if ! $dryrun && [[ ! -z "${pkgs}" ]]; then
  sudo apt-get -y remove --purge ${pkgs}
fi

# Remove errors from previous run, if any.
if [[ -e ./pkg-maintain-no-candidates ]]; then
  rm ./pkg-maintain-no-candidates
fi

# Handle installs
pkgs=""
while read line; do
  if ! pkg-is-installed "$line"; then
    if pkg-has-candidate "$line"; then
      errcho "Schedulling install: $line"
      pkgs="${pkgs} $line" 
    else
      errcho "No candidate: $line"
      echo "$line" >> pkg-maintain-no-candidates
    fi
  else
    errcho "Package already installed: $line"
  fi    
done < <(cat "$installs" | uncomment)
if ! $dryrun && [[ ! -z "${pkgs}" ]]; then
  sudo apt-get -y install ${pkgs}
fi

# This is a success if we didn't forget
# any package.
# shellcheck disable=SC2046
exit $(test ! -e ./pkg-maintain-no-candidates)
