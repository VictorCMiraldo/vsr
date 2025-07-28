{config, pkgs, ...}:
let
  my-theme = import ../theme.nix;
in {
  home.file.".config/wofi/style.css".text = 
    ''
      *{
          font-family: ${my-theme.fonts.normal-name};
          color: ${my-theme.colors.active-text};
      }
      window {
          background-color: ${my-theme.colors.unfocused-bg};
      }
      #input {
          margin: 5px;
          border-radius: 0px;
          border: none;
          border-bottom: 3px solid ${my-theme.colors.inactive-border};
          background-color: ${my-theme.colors.unfocused-bg};
          color: ${my-theme.colors.active-text};
      }
      #inner-box {
          background-color: ${my-theme.colors.unfocused-bg};
      }
      #outer-box {
          margin: 5px;
          padding:2px;
          background-color: ${my-theme.colors.inactive-bg};
      }

      #scroll { }

      #text {
        padding: 5px;
        color: ${my-theme.colors.inactive-text};
      }

      // #entry:nth-child(even){
      //     background-color: ${my-theme.colors.unfocused-bg};
      // }

      #entry:selected {
          background-color: ${my-theme.colors.active-bg};
          border: ${my-theme.colors.active-border};
      }

      #text:selected {
          color: ${my-theme.colors.active-text};
      }
    '';
}
