Victor's System Resources
=========================

This is a repo organizing my system configuration and
scripts, including a bootstrapping script to help
start up on a fresh install of a debian-based distro.

In `config/` one finds a variety of sytem config files.

In `scripts/` one finds a variety of utility scripts I use often.

Running `install` will do a number of things:
  1) Override the system's defaults with soft links
     to my versions of configs in `config/`
  2) Append a line to the default bash_profile, calling
     my addendun's to it.
  3) Remove the linux default directory structure and
     replace it with mine.

Note that the `install` script will **not** install other
packages in the system. It merely installs `vsr` into the system.

Setting up my env on a fresh install
====================================

To install my full config in a freshly-installed distro is pretty simple.
First, run `vsr-generate-sys-startup`, and copy `~/tmp/sys-bootstrap/` to a pendrive. We will need the data in there to start the system up.

In the fresh machine now, we do:

  1) Extract `vsr.tar.gz` to `~/repos/mine/vsr`
  2) Run `~/repos/mine/vsr/install`
  3) Extract `keychain.tar.gz` to `~/usr/keychain`
  4) log off then on again. Some changes only take place at login
  5) Run `pkg-maintain` to install the necessary software (this takes
     a long time and half the software installations require answering
     yes/no questions, so make sure to stay close by!)
  6) log off then on again.
  7) Generate an ssh key for the new machine (we need `pass` for this,
     hence step 5 needs to happen before) register this key
     within the vps. You should have root access to the vps anyway (go look
     inside the keychain and `.../vsr/config/ssh/config`).
  8) Run `vsr-unison`
  9) The system should be mainly set-up, now we need
     some additional software.

## Emacs

  Run

  `cd ~ && git clone --recursive git@github.com:VictorCMiraldo/victor-emacs-config.git .emacs.d`

  Start emacs and let it do its magic. It will probably complain about not finding `agda-mode`, we'll fix that soon.

## Agda

  Install `haskell-stack`, then run `stack install cabal-install`, finally,
  we do the cabal dance:

  ```cabal install hsc2hs alex happy Agda```

  Now, grab a coffee...

## Xmonad

  Clone the repo:

  `cd ~ && git clone --recursive git@github.com:VictorCMiraldo/my-xmonad.git .xmonad`

  Run the installation script.
