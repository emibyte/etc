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

  # programs.rofi.package = pkgs.rofi-wayland;

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
      menu = "rofi -show run";
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

      startup = [
      { command = "systemctl --user import-environment"; }
      { command = "autotiling"; }
      {
        command = let
          setWallpaper = pkgs.writeShellScript "set-wallpaper" ''
            ${pkgs.killall}/bin/killall swaybg
            ${pkgs.swaybg}/bin/swaybg -m fill -i ${/home/fevy/wps/LaptopWPs/wallhaven-85prg1_2560x1600.png}
          '';
          in "${setWallpaper}";
          always = true;
      }
      ];
      window.border = 3;
      window.titlebar = false;
      gaps.inner = 10;
    };
    extraConfig = ''
      for_window [class=".*"] opacity 0.9
      for_window [app_id=".*"] opacity 0.9
    '';
  };
}
