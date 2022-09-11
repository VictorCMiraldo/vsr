{config, pkgs, lib, ...}:
{
  options.vsr = {
    # Whether we're setting up our work machine or not. Some parts of the config
    # such as .git/config and ~/.ssh/config can't be write-protected symlinks for
    # the work computer: company's infrastructure wants to control those files.
    isWorkMachine = lib.mkOption { type = lib.types.bool; default = false; };
  };
}


