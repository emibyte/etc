{pkgs, ...}: let
  cursor = {
    name = "catppuccin-macchiato-pink-cursors";
    package = pkgs.catppuccin-cursors.macchiatoPink;
    size = 48;
  };
in {
  # home.pointerCursor = cursor;
  gtk = {
    enable = true;
    # font = {
    #   name = "Fira Code";
    #   size = 12;
    # };
    theme = {
      name = "catppuccin-frappe-standard-blue-dark";
      package = pkgs.catppuccin-gtk;
    };

    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders.override {
        flavor = "frappe";
        accent = "pink";
      };
    };

    # cursorTheme = cursor;

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk3.extraCss = ''
      decoration { box-shadow: none; }
      decoration:backdrop { box-shadow: none; }
    '';

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };
}
