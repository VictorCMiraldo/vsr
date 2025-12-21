{pkgs, config, ...}:
let
  vsr-sync = pkgs.writeShellApplication {
    name = "vsr-sync";
    runtimeInputs = [ pkgs.unison ];
    text = ''
      if [[ "$#" -ne 1 ]]; then
        echo "Usage: vsr-sync <remote-hostname>"
        exit 1
      fi

      remote_hostname="$1"
      local_root="${config.home.homeDirectory}"
      remote_root="ssh://$remote_hostname:2142/${config.home.homeDirectory}"
      
      for prof in keychain doc; do
        unison -perms 0 "$prof" -root "$local_root" -root "$remote_root"
      done
    '';
  };
in
{
  home.packages = [
    pkgs.unison
    vsr-sync
  ];

  home.file.".unison/common".source = ./common;
  home.file.".unison/doc.prf".source = ./doc.prf;
  home.file.".unison/keychain.prf".source = ./keychain.prf;
}
