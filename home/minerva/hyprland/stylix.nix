{pkgs, ...}: {
  stylix = {
    enable = true;
    targets.rofi.enable = false;
    targets.waybar.enable = false;
    targets.dunst.enable = false;
    targets.hyprland.enable = false;
    targets.hyprlock.enable = false;
    targets.spicetify.enable = false;
  };
}
