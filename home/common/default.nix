{
  config,
  pkgs,
  ...
}: let
  # for this command to work the hostname has to match the a profile in the flake.nix
  rebuild-system = pkgs.writeShellScriptBin "rebuild-system" ''
    nixos-rebuild switch --flake ${config.home.homeDirectory}/etc --verbose
  '';

  # maybe use ghcup instead?
  ghc = pkgs.haskellPackages.ghcWithPackages (hspkgs: [
    hspkgs.cabal-install
    hspkgs.safe
  ]);
in {
  imports = [
    ./wezterm
    ./firefox.nix
    ./zathura.nix
    ./zsh.nix
    ./mpv.nix
    ./languages.nix
  ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "emily";
  home.homeDirectory = "/home/emily";

  fonts.fontconfig.enable = true;

  # Packages that should be installed to the user profile.
  home.packages = [
    rebuild-system

    pkgs.discord
    pkgs.spotify

    pkgs.brave

    pkgs.font-awesome

    pkgs.keepassxc
    pkgs.aseprite

    pkgs.wezterm

    # corsair opensource driver
    # pkgs.ckb-next

    # utils
    pkgs.hyfetch
    pkgs.acpi
    pkgs.unzip
    pkgs.zip
    pkgs.gdb
    pkgs.tree
    # probably dont need it but dunno
    pkgs.autotiling
  ];

  services.sxhkd.enable = true;
  services.network-manager-applet.enable = true;

  programs.git = {
    enable = true;
    userName = "emibyte";
    userEmail = "fevymarine@gmail.com";
  };

  programs.ssh = {
    enable = true;
  };

  programs.htop = {
    enable = true;
    package = pkgs.htop-vim;
    settings = {
      tree_view = true;
      hide_userland_threads = true;
      show_program_path = false;
      highlight_base_name = true;
      color_scheme = 6;
      vim_mode = true;
    };
  };

#  xdg.userDirs = {
#    enable = true;
#    desktop = "$HOME/";
#    documents = "HOME/";
#    download = "$HOME/";
#    music = "$HOME/";
#    pictures = "$HOME/";
#    templates = "$HOME/";
#    videos = "$HOME/";
#  };

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
