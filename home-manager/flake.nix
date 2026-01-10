{
  inputs = {
    nixpkgs.url = "flake:nixpkgs/nixos-25.05";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";
  };
  outputs = { self, nixpkgs, home-manager, agenix, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      homeConfigurations.default = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
                agenix.homeManagerModules.default
                ./home.nix
            ];

            extraSpecialArgs = {
              inherit inputs;
            };
         };
      };
}
