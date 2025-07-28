{
  inputs = {
    # nixpkgs.url = "flake:nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "flake:nixpkgs/nixos-24.11";
    home-manager.url = "github:nix-community/home-manager/release-24.11";

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
