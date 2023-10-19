{
  description = "my system config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tuxedo-nixos = {
      url = "github:blitz/tuxedo-nixos";
      # tcc is tested exclusively against nixpkgs 22.11
      # bcs of that the nixpkgs version is specified as default in the tuxedo-nixos module
      # inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, tuxedo-nixos }:
    let
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
  	    inherit system;
      	    modules = [
	      ./configuration.nix

	      home-manager.nixosModules.home-manager {
	        home-manager.useGlobalPkgs = true;
	        home-manager.useUserPackages = true;
	        home-manager.users.fevy = {
	          imports = [ ./home.nix ];
	        };
	      }

	      tuxedo-nixos.nixosModules.default {
	        hardware.tuxedo-control-center.enable = true;
	        hardware.tuxedo-control-center.package = tuxedo-nixos.packages.x86_64-linux.default;
	      }
	    ];
	  };
        };
      };
}
