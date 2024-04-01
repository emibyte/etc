{pkgs, ...}: let
  myEmacs = pkgs.emacs29-pgtk;
in {
    programs.emacs = {
      enable = true;
      package = myEmacs;
      extraPackages = epkgs: [
          epkgs.vterm
          epkgs.treesit-grammars.with-all-grammars
      ];
    };

    services.emacs = {
      enable = true;
      client = {
        enable = true;
        arguments = ["--create-frame" "--no-wait"];
      };
      socketActivation.enable = true;
    };
}
