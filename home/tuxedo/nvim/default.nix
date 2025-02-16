{pkgs, ...}: {
  home.file."nvim" = {
    source = ./nvim;
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
      vimPlugins.rainbow-delimiters-nvim
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
}
