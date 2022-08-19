let
  my-theme = import ./theme.nix;
in '' 
configuration {
  font: "Hack Nerd Font Medium 10";

  calc {
    display-name: " ";
  }

  drun {
    display-name: " ";
  }

  run {
    display-name: " ";
  }

  window {
    display-name: " ";
  }

  timeout {
    delay: 10;
    action: "kb-cancel";
  }
}''

