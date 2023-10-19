{ config, pkgs, ... }:
let
  # for this command to work the hostname has to match the a profile in the flake.nix
  rebuild-system = pkgs.writeShellScriptBin "rebuild-system" ''
    nixos-rebuild switch --flake ${config.home.homeDirectory}/etc --verbose
  '';
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "fevy";
  home.homeDirectory = "/home/fevy";

  # Packages that should be installed to the user profile.
  home.packages = [
    rebuild-system

    pkgs.keepassxc

    pkgs.acpi
    pkgs.unzip
    pkgs.zip
    pkgs.sqlite
    pkgs.gdb

    # pkgs.fortune
  ];

  # configuration only, setting it as login shell has
  # happen on system level (configuration.nix)
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    oh-my-zsh = {
      enable = true;
      theme = "robbyrussell";
    };
  };

  programs.git = {
   enable = true;
   userName = "FevyFevy";
   userEmail = "fevymarine@gmail.com";
  };

  programs.ssh = {
    enable = true;
  };

  programs.htop = {
    enable = true;
    settings = {
      tree_view = true;
      hide_userland_threads = true;
      show_program_path = false;
      highlight_base_name = true;
      color_scheme = 6;
    };
  };
  
  xdg.userDirs = {
    enable = true;
    desktop = "$HOME/";
    documents = "HOME/";
    download = "$HOME/";
    music = "$HOME/";
    pictures = "$HOME/";
    templates = "$HOME/";
    videos = "$HOME/";
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
