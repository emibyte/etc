{
  description = "my system config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      # url = "github:nix-community/home-manager";
      url = "github:nix-community/home-manager/release-24.11";
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
    pkgs = import nixpkgs {
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
          ./system/tuxedo/configuration.nix
          ./system/emacs.nix

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
          ./system/minerva/configuration.nix

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
