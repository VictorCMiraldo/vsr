VSR
===

This is a repo organizing my system configuration and
scripts, including a bootstrapping script to help
start up on a fresh install of a debian-based distro.

To install on a fresh system run `install.sh` and will perform all the steps
that are required. The actual install is divided in two parts, the system-level install
that will prepare the underlying system and the user-level install, managed by `nix`:

1. The `system/` folder contains the changes that I like to perform
on my underlying system. These include:
    1. Managing a selection of apt packages.
    1. Installing `nix` and `home-manager`
    1. Setting up my `sshd` config
    1. Setting up my `ufw` config
    1. Enabling automatic loggin on lightDM
Under `system/env.sh` we place any environment settings that are needed on this step.
That file will be sourced by my [bash profile](home-manager/programs/bash/profile) later on, 
so we don't loose any of it. The system installation script will produce a 
`system/previous-install.state` file, indicating which steps have already been 
succesfuly performed. Maybe at some point I should
switch to NixOS, but not right now. It's a little hacky but gets the job done.
1. The user-level config is maintained by `home-manager`, hence a mere `home-manager switch` will
do the job.

