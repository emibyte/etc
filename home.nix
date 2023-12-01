{ config, pkgs, ... }:
let
  # for this command to work the hostname has to match the a profile in the flake.nix
  rebuild-system = pkgs.writeShellScriptBin "rebuild-system" ''
    nixos-rebuild switch --flake ${config.home.homeDirectory}/etc --verbose
  '';
  
  ghc = pkgs.haskellPackages.ghcWithPackages (hspkgs: [
    hspkgs.cabal-install
  ]);
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "fevy";
  home.homeDirectory = "/home/fevy";

  # Packages that should be installed to the user profile.
  home.packages = [
    rebuild-system
    ghc

    pkgs.emacs29

    pkgs.keepassxc
    pkgs.aseprite

    pkgs.wezterm

    # utils
    pkgs.hyfetch
    pkgs.acpi
    pkgs.unzip
    pkgs.zip
    pkgs.sqlite
    pkgs.gdb
    pkgs.ripgrep

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

  home.file."wezterm" = {
    source = ./config/wezterm;
    target = ".config/wezterm";
    recursive = true;
  };

  home.file."nvim" = { 
    source = ./config/nvim; 
    target = ".config/nvim"; 
    recursive = true;
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;

    plugins = with pkgs; [
      # languages
      vimPlugins.nvim-lspconfig
      vimPlugins.nvim-treesitter.withAllGrammars
      vimPlugins.rust-tools-nvim
      vimPlugins.vim-cue

      # telescope
      vimPlugins.plenary-nvim
      vimPlugins.popup-nvim
      vimPlugins.telescope-nvim

      # theme
      vimPlugins.catppuccin-nvim

      # floaterm
      vimPlugins.vim-floaterm

      # extras
      # vimPlugins.copilot-lua
      vimPlugins.gitsigns-nvim
      vimPlugins.lualine-nvim
      vimPlugins.nerdcommenter
      vimPlugins.nui-nvim
      vimPlugins.nvim-colorizer-lua
      vimPlugins.nvim-notify
      vimPlugins.nvim-treesitter-context
      vimPlugins.nvim-ts-rainbow2
      vimPlugins.omnisharp-extended-lsp-nvim

      # autocomplete
      vimPlugins.nvim-cmp
      vimPlugins.cmp-nvim-lsp
      vimPlugins.luasnip
      vimPlugins.cmp_luasnip
      vimPlugins.friendly-snippets
      vimPlugins.neodev-nvim
    ];
  };

  # configuration only, setting it as login shell has
  # to happen on system level (configuration.nix)
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
