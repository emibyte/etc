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
  # TODO: move this into a cursor.nix
  getFrom = url: hash: name: {
    name = name;
    size = 32;
    package = pkgs.runCommand "moveUp" {} ''
      mkdir -p $out/share/icons
      ln -s ${pkgs.fetchzip {
        url = url;
        hash = hash;
      }} $out/share/icons/${name}
    '';
  };
  cursorMiku =
    getFrom
    "https://github.com/supermariofps/hatsune-miku-windows-linux-cursors/releases/download/1.2.6/miku-cursor-linux.tar.xz"
    "sha256-qxWhzTDzjMxK7NWzpMV9EMuF5rg9gnO8AZlc1J8CRjY="
    "miku-cursor-linux";
in {
  imports = [
    ./firefox.nix
    ./zathura.nix
    ./shell.nix
    ./mpv.nix
    ./languages.nix
    ./obs.nix
    ./ghostty.nix
    ./xdg.nix
    ./spicetify.nix
    ./floorp.nix
    ./stylix.nix
    ./mangohud.nix
  ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "emily";
  home.homeDirectory = "/home/emily";
  xdg.configFile."emacs".source = config.lib.file.mkOutOfStoreSymlink emacsCfgPath;
  xdg.mimeApps.enable = true;
  xdg.mimeApps.defaultApplications = {
    "inode/directory" = ["thunar.desktop"];
    "application/pdf" = ["org.pwmt.zathura.desktop"];
  };

  fonts.fontconfig.enable = true;

  # Packages that should be installed to the user profile.
  home.packages = [
    rebuild-system
    pkgs.nvibrant-git
    pkgs.lm_sensors
    pkgs.bat

    pkgs.far2l

    pkgs.discord
    pkgs.vesktop

    # pkgs.spotify
    pkgs.vlc
    pkgs.thunderbird-bin

    pkgs.brave

    pkgs.font-awesome

    pkgs.keepassxc
    pkgs.aseprite
    pkgs.desmume
    pkgs.mgba

    pkgs.obsidian # causes an electron rebuild that i dont feel like waiting for
    pkgs.foliate
    pkgs.chatterino2

    # corsair opensource driver
    # pkgs.ckb-next

    # utils
    pkgs.hyfetch
    pkgs.acpi
    pkgs.unzip
    pkgs.zip
    pkgs.gdb
    pkgs.tree
    pkgs.gparted

    pkgs.qpwgraph # audio stuffies
  ];

  services.sxhkd.enable = true;
  services.network-manager-applet.enable = true;

  programs.git = {
    enable = true;
    settings.user.name = "emibyte";
    settings.user.email = "fevymarine@gmail.com";
    settings = {
      pull.rebase = true; # to prevent merge commits on pull
    };
  };

  programs.gh = {
    enable = true;
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

  programs.btop = {
    enable = true;
  };

  # TODO: move this into a cursor.nix
  home.pointerCursor =
    {
      enable = true;
      gtk.enable = true;
      dotIcons.enable = true;
      x11.enable = true;
    }
    // cursorMiku;
  gtk = {
    enable = true;
    cursorTheme = cursorMiku;

    gtk3 = {
      extraConfig = {
        gtk-cursor-theme-size = cursorMiku.size;
        gtk-application-prefer-dark-theme = true;
      };
    };
    gtk4 = {
      extraConfig = {
        gtk-cursor-theme-size = cursorMiku.size;
        gtk-application-prefer-dark-theme = true;
      };
    };
  };

  # qt = {
  #   enable = true;
  #   platformTheme.name = "gtk";
  # };

  home.sessionVariables = {
    NIXOS_OZONE_WL = 1;
    WLR_NO_HARDWARE_CURSORS = 1;
    MOZ_ENABLE_WAYLAND = 1;
  };

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
