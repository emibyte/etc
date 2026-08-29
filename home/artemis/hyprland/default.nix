{pkgs, ...}: {
  home.packages = with pkgs; [hyprshot];

  wayland.windowManager.hyprland = {
    configType = "lua";
    enable = true;
    systemd.variables = ["--all"];
    extraConfig = builtins.readFile ./hyprland.lua;
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [xdg-desktop-portal-hyprland xdg-desktop-portal-gtk];
    config = {
      hyprland = {
        default = ["hyprland"];
        # "org.freedesktop.impl.portal.FileChooser" = ["kde"];
        # "org.freedesktop.impl.portal.FileChooser" = "kde";
        # "org.freedesktop.portal.Settings" = "gtk";
      };
      common = {
        default = ["gtk" "hyprland"];
      };
    };
  };

  services.hyprpolkitagent.enable = true;
}
