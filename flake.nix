{
  description = "my system config";

  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      # url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";

    tuxedo-nixos.url = "github:blitz/tuxedo-nixos";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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
          ./configuration.nix
          ./system/emacs.nix

          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.emily = {
              imports = [
                ./home
              ];
            };
          }

          tuxedo-nixos.nixosModules.default
          {
            hardware.tuxedo-control-center.enable = true;
            hardware.tuxedo-control-center.package = tuxedo-nixos.packages.x86_64-linux.default;
          }
        ];
      };
    };
  };
}
