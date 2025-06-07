{
  pkgs,
  inputs,
  ...
}: let
  cuteEmacs = pkgs.emacs-pgtk;
in {
  # nixpkgs.overlays = [inputs.emacs-overlay.overlay];

  environment.systemPackages = with pkgs; [
    ## Emacs itself
    binutils # native-comp needs 'as', provided by this

    # NOTE: wanted to use emacsWithPackagesFromUsePackage but it only supports configs that consist of one file, meh
    ((emacsPackagesFor cuteEmacs).emacsWithPackages
      (epkgs: [
        epkgs.use-package

        epkgs.vterm
        epkgs.eat

        epkgs.yasnippet
        epkgs.lsp-mode
        epkgs.lsp-ui
        epkgs.projectile
        epkgs.counsel-projectile

        epkgs.ivy
        epkgs.ivy-rich
        epkgs.diminish
        # epkgs.helm
        epkgs.counsel
        epkgs.helpful
        epkgs.hydra
        epkgs.company
        epkgs.company-box
        epkgs.projectile

        epkgs.sicp
        epkgs.tree-sitter-langs
        (epkgs.treesit-grammars.with-grammars
          (grammars: [
            grammars.tree-sitter-bash
            grammars.tree-sitter-c
            grammars.tree-sitter-lua
            grammars.tree-sitter-elisp
            grammars.tree-sitter-ocaml
            grammars.tree-sitter-rust
            grammars.tree-sitter-cpp
            grammars.tree-sitter-scheme
            grammars.tree-sitter-nix
            grammars.tree-sitter-haskell
          ]))

        epkgs.pretty-sha-path
        epkgs.undo-tree
        epkgs.evil
        epkgs.evil-collection
        epkgs.evil-nerd-commenter
        epkgs.rainbow-delimiters
        epkgs.which-key

        epkgs.ef-themes
        epkgs.catppuccin-theme
        epkgs.spacemacs-theme
        epkgs.doom-modeline
        epkgs.doom-themes

        epkgs.nix-mode
        epkgs.org
      ]))

    (ripgrep.override {withPCRE2 = true;})
    gnutls # for TLS connectivity

    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    (aspellWithDicts (ds: with ds; [de en en-computers en-science]))

    editorconfig-core-c # per-project style config

    sqlite

    texlive.combined.scheme-medium
  ];

  services.emacs.enable = true;

  environment.sessionVariables.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];
  fonts.packages = [pkgs.emacs-all-the-icons-fonts];
}
