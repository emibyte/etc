{ pkgs, ... }:
let
  cursor = {
    name = "Catppuccin-Macchiato-Pink-Cursors";
    package = pkgs.catppuccin-cursors.macchiatoPink;
  };
in
{
  home.pointerCursor = cursor;
  gtk = {
    enable = true;
    # font = {
    #   name = "Fira Code";
    #   size = 12;
    # };
    theme = {
      name = "Catppuccin-Frappe-Standard-Blue-Dark";
      package = pkgs.catppuccin-gtk;
    };
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.catppuccin-papirus-folders;
    };
    cursorTheme = {
      name = "Catppuccin-Macchiato-Pink-Cursors";
      package = pkgs.catppuccin-cursors.macchiatoPink;
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk3.extraCss = ''
      decoration { box-shadow: none; }
      decoration: backdrop { box-shadow: none; }
    '';

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };
}
