{
  description = "my system config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tuxedo-nixos.url = "github:blitz/tuxedo-nixos";
    # emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    nixpkgs-unstable,
    home-manager,
    tuxedo-nixos,
    ...
  } @ inputs: let
    inherit (self) outputs;
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    pkgs-unstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };

    lib = nixpkgs.lib;
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;

    nixosConfigurations = {
      tuxedo = lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        inherit system;
        modules = [
          # TODO: change configuration to just default.nix and modularize
          ./system/tuxedo/configuration.nix
          ./system/common

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.emily = {
              imports = [
                ./home/common
                ./home/tuxedo
              ];
            };
          }
        ];
      };
      minerva = lib.nixosSystem {
        specialArgs = {inherit inputs outputs pkgs pkgs-unstable;};
        inherit system;
        modules = [
          # TODO: change configuration to just default.nix and modularize
          ./system/minerva/configuration.nix
          ./system/common
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.emily = {
              imports = [
                ./home/common
                #./home/minerva theres nothing here yet
              ];
            };
          }
        ];
      };
    };
  };
}
