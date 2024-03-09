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
    ./nvim
    ./wezterm
    ./wayland
    ./themes
    ./firefox.nix
    ./dunst.nix
  ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "fevy";
  home.homeDirectory = "/home/fevy";

  fonts.fontconfig.enable = true;

  # Packages that should be installed to the user profile.
  home.packages = [
    rebuild-system
    ghc

    pkgs.font-awesome
    pkgs.emacs29

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
    pkgs.sqlite
    pkgs.gdb
    pkgs.ripgrep
    pkgs.tree

    # pkgs that are (pretty much) exclusively for nvim
    # could be under programs.neovim.extraPackages but not sure
    # if i wanna put them there
    # languages
    pkgs.dotnet-sdk_8
    pkgs.jsonnet
    pkgs.nodejs
    pkgs.python310Full
    pkgs.rustc
    pkgs.go
    pkgs.lua

    # language servers
    pkgs.cuelsp
    pkgs.haskell-language-server
    pkgs.jsonnet-language-server
    pkgs.lua-language-server
    pkgs.nil
    pkgs.nodePackages."bash-language-server"
    pkgs.nodePackages."diagnostic-languageserver"
    pkgs.nodePackages."dockerfile-language-server-nodejs"
    pkgs.nodePackages."pyright"
    pkgs.nodePackages."typescript"
    pkgs.nodePackages."typescript-language-server"
    pkgs.nodePackages."vscode-langservers-extracted"
    pkgs.nodePackages."yaml-language-server"
    pkgs.omnisharp-roslyn
    pkgs.gopls
    pkgs.rust-analyzer
    pkgs.terraform-ls

    # formatters
    pkgs.nixpkgs-fmt
    pkgs.gofumpt
    pkgs.golines
    pkgs.python310Packages.black
    pkgs.rustfmt
    pkgs.terraform

    # tools
    pkgs.cargo
    pkgs.fd
    pkgs.gcc
    pkgs.lazydocker
    pkgs.yarn
  ];

  services.sxhkd.enable = true;

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    settings = {
      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };

      nix_shell = {
        format = "[$symbol $state]($style) ";
        symbol = "❄️";
      };
    };
  };

  # configuration only, setting it as login shell has
  # to happen on system level (configuration.nix)
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;
    oh-my-zsh = {
      enable = true;
      plugins = ["git"];
    };
  };

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
