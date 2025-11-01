# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  ...
}: let
  user = "emily";
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # tuxedo
  # TODO: doesn't work right now tuxedo-keyboard needs to be bumped
  #       there's a pr for that
  hardware.tuxedo-drivers.enable = true;
  hardware.tuxedo-rs = {
    enable = true;
    tailor-gui.enable = true;
  };

  hardware.keyboard.qmk.enable = true;
  services.udev.packages = [pkgs.via];

  stylix = {
    enable = true;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    targets = {
      fontconfig.enable = false;
      chromium.enable = false;
    };

    icons = {
      enable = true;
      package = pkgs.catppuccin-papirus-folders.override {
        flavor = "mocha";
        accent = "pink";
      };
      dark = "Papirus-Dark";
    };
  };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;

  networking.hostName = "tuxedo"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Enable bluetooth (hopefully)
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT = "de_DE.UTF-8";
    LC_MONETARY = "de_DE.UTF-8";
    LC_NAME = "de_DE.UTF-8";
    LC_NUMERIC = "de_DE.UTF-8";
    LC_PAPER = "de_DE.UTF-8";
    LC_TELEPHONE = "de_DE.UTF-8";
    LC_TIME = "de_DE.UTF-8";
  };

  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-media-tags-plugin
      thunar-volman
    ];
  };

  services = {
    # Mount, trash, and other functionalities
    gvfs.enable = true;

    # Thumbnail support for images
    tumbler.enable = true;
  };

  programs.light.enable = true;
  programs.sway.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = ["intel"];

    xkb.layout = "de";
    xkb.variant = "";
  };

  services = {
    displayManager = {
      sddm.wayland.enable = true;
      sddm.package = pkgs.kdePackages.sddm;
      sddm.enable = true;
      sddm.theme = "catppuccin-mocha";
      sddm.enableHidpi = true;
      defaultSession = "sway";
    };
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  environment.sessionVariables.XDG_CONFIG_HOME = "$HOME/.config";

  security.polkit.enable = true;
  security.pam.services.swaylock = {};
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [intel-media-driver intel-ocl intel-vaapi-driver vpl-gpu-rt];
  };

  # Steam stuff, probably make a steam.nix
  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    remotePlay.openFirewall = true; # Open ports in firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    extraCompatPackages = with pkgs; [
      proton-ge-bin
    ];
  };

  programs.gamemode = {
    enable = true;
    settings.general.inhibit_screensaver = 0;
  };

  hardware.xone.enable = true;

  xdg = {
    portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      # config.common.default = "*";
    };
  };

  # Enable corsair driver
  hardware.ckb-next.enable = true;

  # Configure console keymap
  console.keyMap = "de";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # TODO: move to sound.nix file that i still need to create
  # Enable sound with pipewire.
  # sound.enable = false;
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;

    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # TODO: zsh.nix file in system
  # enabling zsh shell
  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${user} = {
    isNormalUser = true;
    description = "emily";
    extraGroups = ["networkmanager" "wheel" "input" "tty" "audio" "video" "openrazer"];
    shell = pkgs.zsh;
    packages = with pkgs; [
      firefox
      #  thunderbird
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    variables = {
      TERMINAL = "ghostty";
      EDITOR = "emacs";
      VISUAL = "emacs";
    };
    # to enable zsh completion for system packages like systemd
    pathsToLink = ["/share/zsh"];
    systemPackages = with pkgs; [
      #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      wget
      curl
      neovim
      git
      killall

      # TODO: move this into a sound.nix file
      pulseaudio
      pavucontrol
      playerctl

      catppuccin-sddm
      libsForQt5.qt5.qtquickcontrols2
      (pkgs.catppuccin-sddm.override {
        flavor = "mocha";
        loginBackground = true;
      })
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  fonts.packages = with pkgs;
    [
      iosevka-comfy.comfy
      noto-fonts
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  nix = {
    settings = {
      auto-optimise-store = true;

      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # keep-outputs and derivations ensures the build time dependencies are only gc'd when the package is gc'd
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };
}
