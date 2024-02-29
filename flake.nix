{
  description = "my system config";

  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";

    };
    nixpkgs-f2k.url = "github:fortuneteller2k/nixpkgs-f2k";

    tuxedo-nixos = {
      url = "github:blitz/tuxedo-nixos";
    };
  };

  outputs = { self, nixpkgs, home-manager, tuxedo-nixos, ... }@inputs:
    let
      inherit (self) outputs;
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      lib = nixpkgs.lib;
    in
    {
      nixosConfigurations = {
        tuxedo = lib.nixosSystem {
          specialArgs = { inherit inputs outputs;};
          inherit system;
          modules = [
            ./configuration.nix

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.fevy = {
                imports = [ 
                    ./home.nix 
                    ./modules/home/wayland/waybar/default.nix
                    ./modules/home/wayland/sway.nix 
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
