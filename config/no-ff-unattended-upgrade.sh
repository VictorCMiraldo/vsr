#! /bin/bash

# Add a line after a match; in this case, we add "firefox"; to the blacklist of unattended upgrades;
# hopefully preventing the annoying side-effect of firefox having to restart in the middle
# of my session
sed -i.bckp '/^Unattended-Upgrade::Package-Blacklist/a   "firefox";' /etc/apt/apt.conf.d/50unattended-upgrades
