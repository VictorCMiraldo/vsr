{
  inputs = {
    # nixpkgs.url = "flake:nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "flake:nixpkgs/nixos-22.11";

    # home-manager.url = "github:nix-community/home-manager";
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    # Follow existing nixpkgs for better caching. This can break things if your
    # versions don't match! In our case we use version 22.11 for both home
    # manager and nixpkgs.
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    { 
      homeConfigurations.default = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [ ./home.nix ]; 
       };

      };
}
