tee ~/.nix-channels | <<EOF
https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz home-manager
https://nixos.org/channels/nixos-22.11 nixpkgs
EOF

nix-channel --update
