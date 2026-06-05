{
  inputs = {
    nixpkgs.url = "flake:nixpkgs/nixos-25.11";

    home-manager.url = "github:nix-community/home-manager/release-25.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    agenix.url = "github:ryantm/agenix";

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      agenix,
      git-hooks,
    }:
    let

      hosts = {
        "kale" = {
          hostModule = ./hosts/kale.nix;
          system = "x86_64-linux";
        };
        "bold-bean" = {
          hostModule = ./hosts/bold-bean.nix;
          system = "x86_64-linux";
        };
        "hetzner" = {
          hostModule = ./hosts/hetzner.nix;
          system = "x86_64-linux";
        };
      };
    in
    {
      devShells.x86_64-linux.default =
        let
          system = "x86_64-linux";
          pkgs = import nixpkgs { inherit system; };
          pre-commit-check = git-hooks.lib.${system}.run {
            src = ./.;
            hooks.nixfmt.enable = true;
            hooks.end-of-file-fixer.enable = true;
          };
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.cachix
            pkgs.nix-tree
            pkgs.nil
          ]
          ++ pre-commit-check.enabledPackages;

          shellHook = pre-commit-check.shellHook;
        };

      homeConfigurations = nixpkgs.lib.mapAttrs (
        hostname:
        { hostModule, system }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};

          modules = [
            ./options.nix
            agenix.homeManagerModules.default
            ./home.nix
            hostModule
            { _module.args = { inherit agenix hostname; }; }
          ];
        }
      ) hosts;
    };
}
