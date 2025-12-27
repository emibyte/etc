# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./monitoring.nix
    ./hyprland.nix
    ./vm.nix
  ];

  # NOTE: this entire thing should probably be refactored into multiple files
  #       especially since it shares a lot of parts with my laptop config

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.auto-optimise-store = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  nix.settings.substituters = [
    "https://cache.nixos.org/"
    "https://nix-community.cachix.org"
    "https://hyprland.cachix.org"
  ];

  nix.settings.trusted-substituters = [
    "https://hyprland.cachix.org"
  ];

  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
  ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  # python uv stuff
  # https://nixos.wiki/wiki/Python#uv
  environment.variables.UV_PYTHON_DOWNLOADS = "never";
  environment.localBinInPath = true;

  stylix = {
    enable = true;
    polarity = "dark";
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
    # NOTE: try sakura.yaml, stella and cupcake
    #       https://tinted-theming.github.io/tinted-gallery/
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/horizon-light.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/unikitty-light.yaml";
    # NOTE: this is supposed to be the hot pink color theme from kde
    # base16Scheme = {
    #   base00 = "f8bbd0";
    #   base01 = "fce4ec";
    #   base02 = "efefef";
    #   base03 = "7f8c8d";
    #   base04 = "dadeda";
    #   base05 = "31363b";
    #   base06 = "eeeeee";
    #   base07 = "fcfcfc";
    #   base08 = "da4453";
    #   base09 = "f67400";
    #   base0A = "c9ce3b";
    #   base0B = "27ae60";
    #   base0C = "11d116";
    #   base0D = "2980b9";
    #   base0E = "e91e63";
    #   base0F = "ab47bc";
    # };
  };

  hardware.i2c.enable = true;

  hardware.keyboard.qmk.enable = true;
  services.udev.packages = [pkgs.via];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [
    "nvidia-drm.fbdev=1"
  ];
  boot.kernelModules = ["coretemp"];

  boot.supportedFilesystems = ["ntfs"];
  fileSystems."/run/media/emily/Seagate Portable Drive" = {
    device = "/dev/disk/by-uuid/58C68B69C68B45EA";
    fsType = "ntfs-3g";
    options = ["users" "nofail" "exec"];
  };

  fileSystems."/run/media/emily/External 1" = {
    device = "/dev/disk/by-uuid/4E1AEA7B1AEA6007";
    fsType = "ntfs-3g";
    options = ["users" "nofail" "exec"];
  };

  fileSystems."/run/media/emily/SteamLib" = {
    device = "/dev/disk/by-uuid/2C4E62404E6202C6";
    fsType = "ntfs-3g";
    options = ["users" "nofail" "exec" "rw" "uid=1000"];
  };

  networking.hostName = "minerva"; # Define your hostname.
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

  # Enable the X11 windowing system.
  # You can disable this if you're only using the Wayland session.
  services.xserver.enable = true;
  services.xserver.videoDrivers = ["nvidia"];
  services.xserver.dpi = 220;
  services.xserver.upscaleDefaultCursor = true;

  services.gvfs.enable = true;
  services.tumbler.enable = true;

  hardware.nvidia = {
    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
    # of just the bare essentials.
    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    open = true;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # Steam stuff, probably make a steam.nix
  programs.steam = {
    enable = true;
    # protontricks.enable = true;
    gamescopeSession.enable = true;
    remotePlay.openFirewall = true; # Open ports in firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    extraPackages = with pkgs; [
      gamescope
      gamemode
    ];
    extraCompatPackages = with pkgs; [
      proton-ge-bin
    ];
  };

  programs.gamemode = {
    enable = true;
    settings.general.inhibit_screensaver = 0;
  };

  hardware.xone.enable = true;

  programs.gpu-screen-recorder.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  services = {
    displayManager = {
      sddm.wayland.enable = true;
      sddm.enable = true;
      sddm.package = pkgs.kdePackages.sddm;
      sddm.theme = "sddm-astronaut-theme";
      sddm.enableHidpi = true;
      defaultSession = "hyprland";
      sddm.extraPackages = with pkgs; [sddm-astronaut];
    };
  };
  # services.desktopManager.plasma6.enable = false;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "eu";
    variant = "";
  };

  # Enable corsair driver
  hardware.ckb-next = {
    enable = true;
    package = pkgs.ckb-next.overrideAttrs (old: {
      cmakeFlags = (old.cmakeFlags or []) ++ ["-DUSE_DBUS_MENU=0"];
    });
  };

  # Enable flatpak for all users
  services.flatpak.enable = true;
  systemd.services.flatpak-repo = {
    wantedBy = ["multi-user.target"];
    path = [pkgs.flatpak];
    script = ''
      flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    '';
  };

  # Configure console keymap
  console.keyMap = "us";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # NOTE: uses mbedtls which is unsecure
  # services.hardware.openrgb.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.emily = {
    isNormalUser = true;
    description = "emily";
    extraGroups = ["networkmanager" "wheel" "libvirtd" "openrazer" "gamemode"];
    packages = [
      # pkgs.kdePackages.kate
      # pkgs.kdePackages.kcalc
      # pkgs.kdePackages.bluedevil
      # pkgs.kdePackages.dolphin
      pkgs.file-roller
      pkgs.keepassxc
      # NOTE: uses mbedtls which is unsecure
      # pkgs.openrgb-with-all-plugins
    ];
  };

  programs.zsh.enable = true;
  users.users.emily.shell = pkgs.zsh;
  users.users.emily.useDefaultShell = true;

  # Install firefox.
  # programs.firefox.enable = true;
  programs.dconf.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    wget
    via

    sddm-astronaut
    gpu-screen-recorder-gtk
  ];
  environment.pathsToLink = ["/share/zsh"];

  environment.variables.EDITOR = "vim";
  environment.variables.TERMINAL = "ghostty";
  environment.variables.XCURSOR_SIZE = 64;
  # environment.variables.GDK_SCALE = "2.2";
  # environment.variables.GDK_DPI_SCALE = "0.4";
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  fonts.packages = with pkgs;
    [
      iosevka-comfy.comfy
      noto-fonts
      inter
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

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
  system.stateVersion = "24.11"; # Did you read the comment?
}
