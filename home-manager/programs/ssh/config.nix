{config, pkgs, lib, ...}:
{
  config = { 
    programs.ssh = lib.mkIf (! config.vsr.isWorkMachine) {
      enable = true;

      # If this is a work machine, and we don't want to touch SSH config, we'd
      # still want to ensure @AddKeysToAgent "yes"@ is present there.
      addKeysToAgent = "yes";

      matchBlocks = {
        "pizero" = {
          hostname = "192.168.2.1";
          user = "pi";
          identityFile = "/home/victor/keychain/vps/PiZero/id_rsa";
        };
      };
    };

    # Have our authorized keys file
    home.file.".ssh/authorized_keys".text = ''
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHSwnBiaBmNkqUJyRfOjnE0rOwb80Bgkko149d/R/pXW victor@beetroot
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO40gphiJ99S0qmbYqLagjuUf9+x7+6khz5CvZe2MpjO victor@blackbean
      ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAW1EheUQ22usnWqIRr0pUlE5QhDFO9mH1nG0JIYgkKo victor@dev-lt-111
      '';

    # Create the systemd agent unit and set up the environment variable for the socket.
    # I'm not using home-manager for this since it will want to run the ssh-agent from within
    # nix, and I just want to run the default one from the system!
    home.sessionVariablesExtra = ''
        export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
    '';

    home.file.".config/systemd/user/ssh-agent.service".text = ''
      [Unit]
      Description=SSH key agent

      [Service]
      Type=simple
      Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket
      ExecStart=/usr/bin/ssh-agent -D -a $SSH_AUTH_SOCK

      [Install]
      WantedBy=default.target
    '';
  };
}

