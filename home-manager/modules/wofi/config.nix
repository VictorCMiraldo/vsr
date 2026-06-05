{
  config,
  pkgs,
  lib,
  ...
}:
let
  wofi-pass = pkgs.callPackage ./wofi-pass.nix { };
in
{
  config = lib.mkIf (!config.vsr.isServer) {
    home.packages = [ wofi-pass ];
    programs.wofi.enable = true;
    programs.wofi.style = with config.lib.stylix.colors; ''
      * {
        font-family: "Hack Nerd Font Mono", monospace;
      }

      window {
        background-color: #${base01};
      }

      #input {
        margin: 5px;
        border-radius: 0px;
        border: none;
        background-color: #${base01};
        color: #${base05};
      }

      #inner-box {
        background-color: #${base00};
      }

      #outer-box {
        margin: 2px;
        padding: 10px;
        background-color: #${base00};
      }

      #scroll {
        margin: 5px;
      }

      #text {
        padding: 4px;
        color: #${base05};
      }

      #entry:nth-child(even) {
        background-color: #${base01};
      }

      #entry:selected {
        background-color: #${base02};
      }

      #text:selected {
        background: transparent;
      }
    '';
  };
}
