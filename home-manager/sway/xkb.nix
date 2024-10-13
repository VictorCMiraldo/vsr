{pkgs, ...}:
{
  # Create a specific keyboard map that remaps alt_gr to super,
  # so it's a litle easier to use sway.
  #
  # TODO: make the name a nix symbol, so we can import it into
  # the sway config nicely!
  home.file.".xkb/symbols/us_alt_gr_remapped_to_super".text = ''
    default partial alphanumeric_keys
    xkb_symbols "basic" {
            include "us"
            name[Group1] = "Modified programmer US";
            key <RALT> { [ Super_L, Super_R ] };
    };
    '';
}



