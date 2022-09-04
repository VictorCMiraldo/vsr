#! /bin/bash
wd="${BASH_SOURCE%/*}"
echo "Installing from ${wd}"

# Load prelude, if not loaded yet
echo "Loading prelude..."
. ${wd}/../prelude

# Load previous install, if any
if [[ -e ${wd}/previous-install.state ]]; then
  echo "Loading previous install session"
  . ${wd}/previous-install.state
fi

# Make sure we can sudo
sudo -k
errcho "I'll need to sudo a fair bit..."
if ! sudo true; then
  errcho "Sorry, aborting."
  exit 1
fi

# Load system environment variables
echo "Loading environment..."
. ${wd}/env.sh

set -eo pipefail

# Manage system packages
if [[ "${VSR_SYSTEM_PKG_MAINTAIN}" -ne "1" ]]; then
  echo "Managing apt packages..."
  chmod +x "${wd}/packages/pkg-maintain.sh"
  ${wd}/packages/pkg-maintain.sh --verbose --update
  echo "VSR_SYSTEM_PKG_MAINTAIN=1" >> ${wd}/previous-install.state
fi

# Install nix and home-manager
if [[ "${VSR_SYSTEM_NIX}" -ne "1" ]]; then
  echo "Installing nix..."
  chmod +x "${wd}/nix/install.sh"
  ${wd}/nix/install.sh
  echo "VSR_SYSTEM_NIX=1" >> ${wd}/previous-install.state
fi
 
# Configure ssh
if [[ "${VSR_SYSTEM_SSHD}" -ne "1" ]]; then
  echo "Configuing sshd..."
  mkdir -p /etc/ssh/ && \
    sudo cp $(readlink -f "${wd}/sshd/sshd_config") /etc/ssh/sshd_config && \
    sudo chwon root:root /etc/ssh/sshd_config && \
    sudo chmod 0644 /etc/ssh/sshd_config
  echo "VSR_SYSTEM_SSHD=1" >> ${wd}/previous-install.state
fi

# Configure the firewall
if [[ "${VSR_SYSTEM_UFW}" -ne "1" ]]; then
  echo "Configuring ufw..."
  vsr-ufw
  echo "VSR_SYSTEM_UFW=1" >> ${wd}/previous-install.state
fi

# Configure lightdm autologin
if [[ "${VSR_LIGHTDM_AUTOLOGIN}" -ne "1" ]]; then
  echo "Configuring lightDM auto-login"
  if [ ! -d "/etc/lightdm" ]; then
    echo "LightDM doesn't seem to be installed!"
  else
    if [ -f "/etc/lightdm/lightdm.conf" ]; then
      echo "Backing up old lightdm.conf"
      sudo mv /etc/lightdm/lightdm.conf /etc/lightdm/lightdm.conf.bckp
    fi
  
    echo -n "autologin-user=victor\nautologin-user-timeout=0" | sudo tee /etc/lightdm/lightdm.conf
    echo "VSR_LIGHTDM_AUTOLOGIN=1" >> ${wd}/previous-install.state
  fi
fi

echo "Done"

