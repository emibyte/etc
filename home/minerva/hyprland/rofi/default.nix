{
  pkgs,
  lib,
  ...
}: {
  # home.packages = with pkgs; [kdePackages.breeze-icons];
  programs.rofi = {
    enable = true;
    theme = lib.mkForce ./catppuccin-macchiato.rasi;
    package = pkgs.rofi-wayland;
    modes = ["drun" "run"];
    location = "center";
    font = "Iosevka Comfy 16";
    extraConfig = {
      show-icons = true;
      case-sensitive = false;
      display-drun = "   Apps ";
      drun-display-format = "{icon} {name}";
      sidebar-mode = true;
      border-radius = 10;
      # icon-theme = "Breeze-Dark";
    };
  };
}
