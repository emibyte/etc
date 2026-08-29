{
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./rofi
    ./waybar
    ./dunst.nix
    ./stylix.nix
    # ./theme.nix
  ];

  home.packages = with pkgs; [hyprshot];

  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "logout";
        action = "hyprctl dispatch exit 0";
        text = "Log Out";
        keybind = "l";
      }
    ];
  };

  wayland.windowManager.hyprland = {
    configType = "lua";
    enable = true;
    systemd.variables = ["--all"];
    extraConfig = builtins.readFile ./hyprland.lua;
  };

  # FIXME: no idea if this works
  #        https://wiki.hypr.land/Hypr-Ecosystem/xdg-desktop-portal-hyprland/
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

  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        hide_cursor = true;
        ignore_empty_input = true;
      };

      animations = {
        enabled = true;
        fade_in = {
          duration = 300;
          bezier = "easeOutQuint";
        };
        fade_out = {
          duration = 300;
          bezier = "easeOutQuint";
        };
      };

      background = [
        {
          path = "screenshot";
          blur_passes = 3;
          blur_size = 8;
        }
      ];

      input-field = [
        {
          size = "200, 50";
          position = "0, -80";
          monitor = "";
          dots_center = true;
          fade_on_empty = false;
          font_color = "rgb(202, 211, 245)";
          inner_color = "rgb(91, 96, 120)";
          outer_color = "rgb(24, 25, 38)";
          outline_thickness = 5;
          # placeholder_text = "";
          shadow_passes = 2;
        }
      ];
    };
  };

  services.hypridle = {
    enable = false;
    settings.general = {};
  };

  services.hyprpaper = {
    enable = true;
    settings = {
      splash = false;
      preload = [
        "${config.home.homeDirectory}/WPs/GT8rHAXXQAAQ6_5.jpg"
        "${config.home.homeDirectory}/WPs/wallhaven-wewl9r_2560x1600.png"
      ];
      wallpaper = [
        {
          monitor = "DP-2";
          path = "${config.home.homeDirectory}/WPs/GT8rHAXXQAAQ6_5.jpg";
        }
        {
          monitor = "DP-3";
          path = "${config.home.homeDirectory}/WPs/GT8rHAXXQAAQ6_5.jpg";
        }
      ];
    };
  };
}
