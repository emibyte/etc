{ pkgs, ... }:
{

  imports = [ ./rofi.nix ];

  home.packages = with pkgs; [
    swaylock
    swayidle
    wl-clipboard
    mako
    wofi
    waybar
    swaybg

    wdisplays
    wlr-randr
  ];

  programs.rofi.package = pkgs.rofi-wayland;

  home.sessionVariables = {
    NIXOS_OZONE_WL = 1;

    SDL_VIDEODRIVER = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XDG_SESSION_TYPE = "wayland";
  };

  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = "Mod4";
      terminal = "wezterm";
      menu = "wofi --show run";
      bars = [{ command = "${pkgs.waybar}/bin/waybar"; }];
      # output = {
      #   eDP-1 = {
      #     scale = "1";
      #   };
      # };
      input = {
        "type:keyboard" = {
          xkb_layout = "de";
          # xkb_options = "caps:swapescape";
          xkb_numlock = "enabled";

          repeat_delay = "300";
          repeat_rate = "50";
        };
      };

      startup = [{ command = "systemctl --user import-environment"; }];
    };
  };
}
