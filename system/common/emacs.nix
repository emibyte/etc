{
  pkgs,
  inputs,
  ...
}: let
  cuteEmacs = pkgs.emacs-unstable-pgtk;
in {
   nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

  environment.systemPackages = with pkgs; [
    ## Emacs itself
    binutils # native-comp needs 'as', provided by this

    (pkgs.emacsWithPackagesFromUsePackage {
      package = cuteEmacs;
      config = ../../home/emacs/init.el;

      # Optionally provide extra packages not in the configuration file.
      extraEmacsPackages = epkgs: [
        epkgs.use-package
        epkgs.vterm
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
      ];

      # Optionally override derivations.
      # override = epkgs: epkgs // {
      #  somePackage = epkgs.melpaPackages.somePackage.overrideAttrs(old: {
           # Apply fixes here
      #  });
      # };
    })

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
