{
  pkgs,
  inputs,
  ...
}: let
  cuteEmacs = pkgs.emacs29-pgtk;
in {
  # nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

  environment.systemPackages = with pkgs; [
    ## Emacs itself
    binutils # native-comp needs 'as', provided by this
    # idk if this even works i found it in the nixos manual
    ((emacsPackagesFor cuteEmacs).emacsWithPackages
      (epkgs: [epkgs.vterm epkgs.sicp]))

    ## Doom dependencies
    (ripgrep.override {withPCRE2 = true;})
    gnutls # for TLS connectivity

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    ## Module dependencies
    # :checkers spell
    (aspellWithDicts (ds: with ds; [de en en-computers en-science]))
    # :tools editorconfig
    editorconfig-core-c # per-project style config
    # :tools lookup & :lang org +roam
    sqlite
    # :lang latex & :lang org (latex previews)
    texlive.combined.scheme-medium
  ];

  services.emacs.enable = true;

  environment.sessionVariables.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];
  fonts.packages = [pkgs.emacs-all-the-icons-fonts];

  system.userActivationScripts = {
    installDoomEmacs = ''
      if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
        ${pkgs.git}/bin/git clone --depth=1 --single-branch "https://github.com/doomemacs/doomemacs" "$XDG_CONFIG_HOME/emacs"
      fi
    '';
  };
}
