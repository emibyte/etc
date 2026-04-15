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
        epkgs.vterm-toggle
        epkgs.eat

        epkgs.yasnippet
        epkgs.yasnippet-snippets
        epkgs.yasnippet-capf

        epkgs.projectile
        epkgs.perspective
        epkgs.vertico
        epkgs.corfu
        epkgs.corfu-terminal
        epkgs.cape
        epkgs.consult
        epkgs.consult-projectile
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
            grammars.tree-sitter-cmake
            grammars.tree-sitter-c
            grammars.tree-sitter-cpp
            grammars.tree-sitter-lua
            grammars.tree-sitter-fennel
            # grammars.tree-sitter-go ;; go-ts-mode is ass
            # grammars.tree-sitter-gomod
            grammars.tree-sitter-hyprlang
            grammars.tree-sitter-json
            grammars.tree-sitter-javascript
            grammars.tree-sitter-typescript
            grammars.tree-sitter-tsx
            grammars.tree-sitter-vue
            grammars.tree-sitter-python
            grammars.tree-sitter-yaml
            grammars.tree-sitter-toml
            grammars.tree-sitter-make
            grammars.tree-sitter-elisp
            grammars.tree-sitter-ocaml
            grammars.tree-sitter-rust
            grammars.tree-sitter-scheme
            grammars.tree-sitter-nix
            grammars.tree-sitter-haskell
            grammars.tree-sitter-html
            grammars.tree-sitter-css
            grammars.tree-sitter-markdown
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
        epkgs.doom-themes
        epkgs.gruber-darker-theme
        epkgs.moe-theme
        epkgs.kaolin-themes
        epkgs.inkpot-theme
        epkgs.stimmung-themes
        epkgs.naysayer-theme

        epkgs.magit

        epkgs.nix-mode
        epkgs.markdown-mode
        epkgs.org
        epkgs.org-bullets
        epkgs.visual-fill-column
        epkgs.racket-mode
        epkgs.go-mode
        epkgs.tuareg
        epkgs.ocp-indent

        epkgs.minions
        epkgs.doom-modeline

        epkgs.nerd-icons
        epkgs.nerd-icons-dired
        epkgs.nerd-icons-ibuffer
        epkgs.nerd-icons-corfu
        epkgs.sideline-flymake
        epkgs.eldoc-box
        epkgs.ligature
      ]))

    emacs-lsp-booster
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
