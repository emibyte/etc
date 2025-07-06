{config, ...}: let
in {
  imports = [
    ./rofi
    ./waybar
    ./dunst.nix
  ];

  wayland.windowManager.hyprland = {
    enable = true;
    systemd.variables = ["--all"];
    settings = {
      monitor = [
        "DP-2, 2560x1400, 0x0, 1" # oled 1440p
        "DP-3, 3840x2160, -3840x0, 1.5" # ips 4k
      ];

      xwayland = {
        force_zero_scaling = true;
      };

      windowrule = [
        "opacity 0.0 override, class:^(xwaylandvideobridge)$"
        "noanim, class:^(xwaylandvideobridge)$"
        "noinitialfocus, class:^(xwaylandvideobridge)$"
        "maxsize 1 1, class:^(xwaylandvideobridge)$"
        "noblur, class:^(xwaylandvideobridge)$"
        "nofocus, class:^(xwaylandvideobridge)$"

        "suppressevent maximize, class:.*"
        "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"
      ];

      workspace = [
        "workspace=1,monitor:DP-2"
        "workspace=2,monitor:DP-2"
        "workspace=3,monitor:DP-2"
        "workspace=4,monitor:DP-2"
        "workspace=5,monitor:DP-2"
        "workspace=6,monitor:DP-2"
        "workspace=7,monitor:DP-2"
        "workspace=8,monitor:DP-2"
        "workspace=9,monitor:DP-2"
        "workspace=10,monitor:DP-2"

        "workspace=11,monitor:DP-3"
        "workspace=12,monitor:DP-3"
        "workspace=13,monitor:DP-3"
        "workspace=14,monitor:DP-3"
        "workspace=15,monitor:DP-3"
        "workspace=16,monitor:DP-3"
        "workspace=17,monitor:DP-3"
        "workspace=18,monitor:DP-3"
        "workspace=19,monitor:DP-3"
        "workspace=20,monitor:DP-3"
      ];

      "$terminal" = "ghostty";

      exec-once = [
        "xwaylandvideobridge"
        "waybar"
      ];

      source = "./macchiato.conf";

      general = {
        gaps_in = "5";
        gaps_out = "10";

        border_size = "2";

        # https://wiki.hypr.land/Configuring/Variables/#variable-types for info about colors
        col.active_border = "$teal";
        col.inactive_border = "$surface1";

        # Set to true enable resizing windows by clicking and dragging on borders and gaps
        resize_on_border = false;

        # Please see https://wiki.hypr.land/Configuring/Tearing/ before you turn this on
        allow_tearing = false;

        layout = "dwindle";
      };

      decoration = {
        rounding = "10";
        rounding_power = "2";

        # https://wiki.hypr.land/Configuring/Variables/#blur
        blur = {
          size = "8";
          passes = "2";
        };

        # Change transparency of focused and unfocused windows
        active_opacity = "1.0";
        inactive_opacity = "1.0";

        shadow = {
          enabled = true;
          range = "4";
          render_power = "3";
          color = "$teal";
          color_inactive = "Oxff$baseAlpha";
        };
      };

      animations = {
        enabled = "yes";

        # Default animations, see https://wiki.hypr.land/Configuring/Animations/ for more

        bezier = [
          "easeOutQuint,0.23,1,0.32,1"
          "easeInOutCubic,0.65,0.05,0.36,1"
          "linear,0,0,1,1"
          "almostLinear,0.5,0.5,0.75,1.0"
          "quick,0.15,0,0.1,1"
        ];

        animation = [
          "global, 1, 10, default"
          "border, 1, 5.39, easeOutQuint"
          "windows, 1, 4.79, easeOutQuint"
          "windowsIn, 1, 4.1, easeOutQuint, popin 87%"
          "windowsOut, 1, 1.49, linear, popin 87%"
          "fadeIn, 1, 1.73, almostLinear"
          "fadeOut, 1, 1.46, almostLinear"
          "fade, 1, 3.03, quick"
          "layers, 1, 3.81, easeOutQuint"
          "layersIn, 1, 4, easeOutQuint, fade"
          "layersOut, 1, 1.5, linear, fade"
          "fadeLayersIn, 1, 1.79, almostLinear"
          "fadeLayersOut, 1, 1.39, almostLinear"
          "workspaces, 1, 1.94, almostLinear, fade"
          "workspacesIn, 1, 1.21, almostLinear, fade"
          "workspacesOut, 1, 1.94, almostLinear, fade"
        ];
      };

      # See https://wiki.hypr.land/Configuring/Dwindle-Layout/ for more
      dwindle = {
        pseudotile = true; # Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
        preserve_split = true; # You probably want this
        smart_split = true;
      };

      # See https://wiki.hypr.land/Configuring/Master-Layout/ for more
      master = {
        new_status = "master";
      };

      # https://wiki.hypr.land/Configuring/Variables/#misc
      misc = {
        force_default_wallpaper = "-1"; # Set to 0 or 1 to disable the anime mascot wallpapers
        disable_hyprland_logo = false; # If true disables the random hyprland logo / anime girl background. :(
        disable_splash_rendering = true;
        background_color = "0x24273a";
      };

      # https://wiki.hypr.land/Configuring/Variables/#input
      input = {
        kb_layout = "eu";

        follow_mouse = "1";

        sensitivity = "0"; # -1.0 - 1.0, 0 means no modification.

        touchpad = {
          natural_scroll = false;
        };
      };

      # https://wiki.hypr.land/Configuring/Variables/#gestures
      gestures = {
        workspace_swipe = false;
      };

      # Example per-device config
      # See https://wiki.hypr.land/Configuring/Keywords/#per-device-input-configs for more
      device = {
        name = "epic-mouse-v1";
        sensitivity = "-0.5";
      };

      binds = {
        workspace_back_and_forth = true;
      };

      "$mainMod" = "SUPER";

      bind = [
        "$mainMod, RETURN, exec, $terminal"
        "$mainMod, C, killactive,"
        "$mainMod, D, exec, rofi -show drun"
        "$mainMod, M, exit,"
        "$mainMod, E, exec, dolphin"
        "$mainMod, V, togglefloating,"
        "$mainMod, P, pseudo, # dwindle"
        "$mainMod, J, togglesplit," # dwindle
        "$mainMod, ESCAPE, exec, wlogout"

        "$mainMod, left, movefocus, l"
        "$mainMod, right, movefocus, r"
        "$mainMod, up, movefocus, u"
        "$mainMod, down, movefocus, d"

        "$mainMod, 1, workspace, 1"
        "$mainMod, 2, workspace, 2"
        "$mainMod, 3, workspace, 3"
        "$mainMod, 4, workspace, 4"
        "$mainMod, 5, workspace, 5"
        "$mainMod, 6, workspace, 6"
        "$mainMod, 7, workspace, 7"
        "$mainMod, 8, workspace, 8"
        "$mainMod, 9, workspace, 9"
        "$mainMod, 0, workspace, 10"

        "$mainMod ALT, 1, workspace, 11"
        "$mainMod ALT, 2, workspace, 12"
        "$mainMod ALT, 3, workspace, 13"
        "$mainMod ALT, 4, workspace, 14"
        "$mainMod ALT, 5, workspace, 15"
        "$mainMod ALT, 6, workspace, 16"
        "$mainMod ALT, 7, workspace, 17"
        "$mainMod ALT, 8, workspace, 18"
        "$mainMod ALT, 9, workspace, 19"
        "$mainMod ALT, 0, workspace, 20"

        "$mainMod SHIFT, 1, movetoworkspace, 1"
        "$mainMod SHIFT, 2, movetoworkspace, 2"
        "$mainMod SHIFT, 3, movetoworkspace, 3"
        "$mainMod SHIFT, 4, movetoworkspace, 4"
        "$mainMod SHIFT, 5, movetoworkspace, 5"
        "$mainMod SHIFT, 6, movetoworkspace, 6"
        "$mainMod SHIFT, 7, movetoworkspace, 7"
        "$mainMod SHIFT, 8, movetoworkspace, 8"
        "$mainMod SHIFT, 9, movetoworkspace, 9"
        "$mainMod SHIFT, 0, movetoworkspace, 10"

        "$mainMod ALT SHIFT, 1, movetoworkspace, 11"
        "$mainMod ALT SHIFT, 2, movetoworkspace, 12"
        "$mainMod ALT SHIFT, 3, movetoworkspace, 13"
        "$mainMod ALT SHIFT, 4, movetoworkspace, 14"
        "$mainMod ALT SHIFT, 5, movetoworkspace, 15"
        "$mainMod ALT SHIFT, 6, movetoworkspace, 16"
        "$mainMod ALT SHIFT, 7, movetoworkspace, 17"
        "$mainMod ALT SHIFT, 8, movetoworkspace, 18"
        "$mainMod ALT SHIFT, 9, movetoworkspace, 19"
        "$mainMod ALT SHIFT, 0, movetoworkspace, 20"

        "$mainMod, S, togglespecialworkspace, magic"
        "$mainMod SHIFT, S, movetoworkspace, special:magic"

        "$mainMod, mouse_down, workspace, e+1"
        "$mainMod, mouse_up, workspace, e-1"
      ];

      bindm = [
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];

      # Requires playerctl
      bindl = [
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
      ];
    };
  };

  # FIXME: no idea if this works
  #        https://wiki.hypr.land/Hypr-Ecosystem/xdg-desktop-portal-hyprland/
  xdg.portal.config = {
    hyprland = {
      default = ["hyprland" "gtk"];
      "org.freedesktop.impl.portal.FileChooser" = ["kde"];
    };
  };

  services.hyprpolkitagent.enable = true;

  services.hypridle.enable = true;

  services.hyprpaper = {
    enable = true;
    settings = {
      preload = [
        "${config.home.homeDirectory}/WPs/GT8rHAXXQAAQ6_5.jpg"
        "${config.home.homeDirectory}/WPs/wallhaven-wewl9r_2560x1600.png"
      ];
      wallpaper = [
        "DP-2,${config.home.homeDirectory}/WPs/GT8rHAXXQAAQ6_5.jpg"
        "DP-3,${config.home.homeDirectory}/WPs/GT8rHAXXQAAQ6_5.jpg"
      ];
    };
  };
}
