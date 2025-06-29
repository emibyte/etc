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
        epkgs.flycheck
        epkgs.quick-peek

        epkgs.projectile
        epkgs.perspective
        epkgs.vertico
        epkgs.corfu
        epkgs.corfu-terminal
        epkgs.cape
        epkgs.kind-icon
        epkgs.consult
        epkgs.consult-projectile
        epkgs.consult-lsp
        epkgs.marginalia
        epkgs.orderless
        epkgs.embark
        epkgs.embark-consult

        epkgs.helpful
        epkgs.hl-todo

        epkgs.hydra
        epkgs.projectile
        epkgs.direnv
        epkgs.ripgrep

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
        epkgs.gruber-darker-theme
        epkgs.moe-theme
        epkgs.kaolin-themes
        epkgs.inkpot-theme
        epkgs.stimmung-themes

        epkgs.magit

        epkgs.nix-mode
        epkgs.org
        epkgs.org-bullets
        epkgs.visual-fill-column
        epkgs.racket-mode
      ]))

    ripgrep
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
  fonts.packages = with pkgs; [emacs-all-the-icons-fonts cantarell-fonts];
}
