{
  description = "my system config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs-overlay.url = "github:nix-community/emacs-overlay";
    spicetify-nix.url = "github:Gerg-L/spicetify-nix";

    stylix = {
      url = "github:nix-community/stylix/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    helium = {
      url = "github:AlvaroParker/helium-nix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    noctalia ={
     url = "github:noctalia-dev/noctalia-shell"; 
     inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    stylix,
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
        modules = [
          {
            nixpkgs.overlays = [self.overlays.additions];
          }
          stylix.nixosModules.stylix

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
              ];
            };
            home-manager.extraSpecialArgs = {inherit inputs;};
          }
          {nixpkgs.hostPlatform = {system = system;};}
        ];
      };
      minerva = lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          {
            nixpkgs.overlays = [self.overlays.additions];
          }
          stylix.nixosModules.stylix

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
              ];
            };
            home-manager.extraSpecialArgs = {inherit inputs;};
          }
          {nixpkgs.hostPlatform = {system = system;};}
        ];
      };
      artemis = lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          {
            nixpkgs.overlays = [
              self.overlays.additions
              # TODO(emi): remove this when stable pkgs get updated (causes build error with newer kernels)
              (final: prev: {
                linuxPackages_latest = prev.linuxPackages_latest.extend (lfinal: lprev: {
                  openrazer = let
                    unstablePkgs = import inputs.nixpkgs-unstable {inherit (final) system;};
                  in
                    lprev.openrazer.overrideAttrs (old: {
                      inherit (unstablePkgs.linuxPackages_latest.openrazer) src version;
                    });
                });
              })
            ];
          }
          stylix.nixosModules.stylix

          ./system/common
          ./system/artemis/configuration.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.backupFileExtension = "hm-backup";
            home-manager.users.emily = {
              imports = [
                ./home/common
                inputs.spicetify-nix.homeManagerModules.default
              ];
            };
            home-manager.extraSpecialArgs = {inherit inputs;};
          }
          {nixpkgs.hostPlatform = {system = system;};}
        ];
      };
    };
  };
}
