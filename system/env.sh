#! /bin/bash

# This file contains some variables that are needed at system instalation time,
# it should ideally be sourced or copied into a bashrc

export VSR_SSH_PORT=2142

# I'll export a HOSTNAME var to make this easily accessible through
# `builtins.getnEnv "HOSTNAME` in home-manager. This adds some important
# stuff such as disabling the tray on polybar for my work machine.
export HOSTNAME=$(hostname)
