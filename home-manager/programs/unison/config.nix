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

      # Tell unison where to find the remote unison binary
      remote_unison="${config.home.homeDirectory}/.nix-profile/bin/unison"

      unison -perms 0 keychain -root "$local_root" -root "$remote_root" \
        -servercmd="$remote_unison" -addversionno=false

      unison -perms 0 doc -root "$local_root" -root "$remote_root" \
        -servercmd="$remote_unison" -addversionno=false
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
