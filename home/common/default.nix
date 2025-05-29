{
  config,
  pkgs,
  ...
}: let
  # for this command to work the hostname has to match the a profile in the flake.nix
  rebuild-system = pkgs.writeShellScriptBin "rebuild-system" ''
    nixos-rebuild switch --flake ${config.home.homeDirectory}/etc --verbose
  '';
  emacsCfgPath = "${config.home.homeDirectory}/etc/home/emacs";
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
  xdg.configFile."emacs".source = config.lib.file.mkOutOfStoreSymlink emacsCfgPath;

  fonts.fontconfig.enable = true;

  # Packages that should be installed to the user profile.
  home.packages = [
    rebuild-system

    pkgs.discord
    pkgs.spotify
    pkgs.vlc

    pkgs.brave

    pkgs.font-awesome

    pkgs.keepassxc
    pkgs.aseprite

    pkgs.wezterm

    pkgs.obsidian
    pkgs.foliate

    # corsair opensource driver
    # pkgs.ckb-next

    # utils
    pkgs.hyfetch
    pkgs.acpi
    pkgs.unzip
    pkgs.zip
    pkgs.gdb
    pkgs.tree
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

  home.pointerCursor = let
    getFrom = url: hash: name: {
      gtk.enable = true;
      x11.enable = true;
      name = name;
      size = 24;
      package = pkgs.runCommand "moveUp" {} ''
        mkdir -p $out/share/icons
        ln -s ${pkgs.fetchzip {
          url = url;
          hash = hash;
        }} $out/share/icons/${name}
      '';
    };
  in
    getFrom
    "https://github.com/supermariofps/hatsune-miku-windows-linux-cursors/releases/download/1.2.6/miku-cursor-linux.tar.xz"
    "sha256-qxWhzTDzjMxK7NWzpMV9EMuF5rg9gnO8AZlc1J8CRjY="
    "miku-cursor-linux";

  # for vm stuff
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
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
