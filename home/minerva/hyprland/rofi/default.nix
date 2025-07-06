{pkgs, ...}: {
  programs.rofi = {
    enable = true;
    theme = ./catppuccin-macchiato.rasi;
    package = pkgs.rofi-wayland;
    modes = ["drun"];
    location = "center";
    font = "Iosevka Comfy 16";
    extraConfig = {
      show-icons = true;
      case-sensitive = false;
      display-drun = "   Apps ";
      drun-display-format = "{icon} {name}";
      sidebar-mode = true;
      border-radius = 10;
    };
  };
}
