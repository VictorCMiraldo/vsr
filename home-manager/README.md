# Notes to self:

* Our [`.profile`](programs/bash/profile) will source three important files:
  - `$HOME/.nix-profile/etc/profile.d/nix.sh`: brings in nix into this shell;
  - `$HOME/.nix-profile/etc/profile.d/hm-session-vars`: brings in the variables 
    declared in the `home.sessionVariables` block, and
  - [`$VSR_ROOT/system/env.sh`](../system/env.sh): brings in the variables that were needed by our
    system installation script. For now, that's just my sshd port.
