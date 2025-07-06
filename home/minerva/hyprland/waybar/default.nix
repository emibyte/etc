{config, ...}: let
  waybarConfigPath = "${config.home.homeDirectory}/etc/home/minerva/hyprland/waybar";
in {
  xdg.configFile."waybar".source = config.lib.file.mkOutOfStoreSymlink waybarConfigPath;
}
