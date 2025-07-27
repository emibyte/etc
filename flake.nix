{
  description = "my system config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs-overlay.url = "github:nix-community/emacs-overlay";
    spicetify-nix.url = "github:Gerg-L/spicetify-nix";
    catppuccin.url = "github:catppuccin/nix";
  };

  outputs = {
    self,
    nixpkgs,
    catppuccin,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
    system = "x86_64-linux";
    lib = nixpkgs.lib;
  in {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    overlays = import ./overlays {};

    nixosConfigurations = {
      tuxedo = lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        inherit system;
        modules = [
          {
            nixpkgs.overlays = [self.overlays.additions];
          }
          catppuccin.nixosModules.catppuccin

          # TODO: change configuration to just default.nix and modularize
          ./system/tuxedo/configuration.nix
          ./system/common

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-backup";
            home-manager.users.emily = {
              imports = [
                ./home/common
                ./home/tuxedo
                inputs.spicetify-nix.homeManagerModules.default
                catppuccin.homeModules.catppuccin
              ];
            };
            home-manager.extraSpecialArgs = {inherit inputs;};
          }
        ];
      };
      minerva = lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        inherit system;
        modules = [
          {
            nixpkgs.overlays = [self.overlays.additions];
          }
          catppuccin.nixosModules.catppuccin

          # TODO: change configuration to just default.nix and modularize
          ./system/minerva/configuration.nix
          ./system/common
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-backup";
            home-manager.users.emily = {
              imports = [
                ./home/common
                ./home/minerva
                inputs.spicetify-nix.homeManagerModules.default
                catppuccin.homeModules.catppuccin
              ];
            };
            home-manager.extraSpecialArgs = {inherit inputs;};
          }
        ];
      };
    };
  };
}
