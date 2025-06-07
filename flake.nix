{
  description = "my system config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tuxedo-nixos.url = "github:blitz/tuxedo-nixos";
    # emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    tuxedo-nixos,
    ...
  } @ inputs: let
    inherit (self) outputs;
    system = "x86_64-linux";
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
        specialArgs = {inherit inputs outputs;};
        inherit system;
        modules = [
          # TODO: change configuration to just default.nix and modularize
          ./system/minerva/configuration.nix
          ./system/common
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "backup";
            home-manager.users.emily = {
              imports = [
                ./home/common
                ./home/minerva
              ];
            };
          }
        ];
      };
    };
  };
}
