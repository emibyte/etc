{
  pkgs,
  config,
  ...
}: let
  colors = {
    pink = "rgb(f5bde6)";
    pinkAlpha = "f5bde6";
    surface1 = "rgb(494d64)";
    surface1Alpha = "494d64";
    baseAlpha = "1e1e2e";
    sapphire = "rgb(7dc4e4)";
    sapphireAlpha = "7dc4e4";
    rosewater = "rgb(f4dbd6)";
    rosewaterAlpha = "f4dbd6";
    # TODO: try out these colors
    # col.active_border=0xffb072d1
    # col.inactive_border=0xff292a37
  };
in {
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
    enable = true;
    systemd.variables = ["--all"];
    settings = {
      monitor = [
        "DP-2, highres@240, 0x0, 1" # oled 1440p 2560x1400@144
        "DP-3, highres@highrr, -2560x0, 1.5" # ips 4k 3840x2160@60 (3840 / 1.5 = 2560)
      ];

      xwayland = {
        enabled = true;
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

        # window rule for steam friends window to be a floating window
        "float,class:^(steam)$,title:Friends List"

        # so that volume control opens as floating window
        "float,class:pwvucontrol,title:pwvucontrol"
        "float,title:Volume Control"
        "size 1400 700,title:Volume Control"
        # (hopefully) no opacity on browsers
        "opacity 1.0 override, class:^(firefox)$"
        "opacity 1.0 override, class:^(floorp)$"
        "opacity 1.0 override, class:^(brave-browser)$"
        "opacity 1.0 override, class:^(chromium-browser)$"
        "opacity 1.0 override, class:^(librewolf)$"
        "opacity 1.0 override, class:^(discord)$"
      ];

      windowrulev2 = [
        "float, class:^(jetbrains-.*),title:^(win.*)"
        "noinitialfocus, opacity 0.9 0.9, class:^(jetbrains-.*)"
        # "noinitialfocus,xwayland:1"
        # "bordercolor ${colors.sapphire},class:*,group:1"
      ];

      workspace = [
        "workspace=1,monitor:DP-2"
        "workspace=2,monitor:DP-3"
        "workspace=3,monitor:DP-2"
        "workspace=4,monitor:DP-2"
        "workspace=5,monitor:DP-2"
        "workspace=6,monitor:DP-2"
        "workspace=7,monitor:DP-2"
        "workspace=8,monitor:DP-3"
        "workspace=9,monitor:DP-2"
        "workspace=10,monitor:DP-3"
      ];

      "$terminal" = "ghostty";

      exec-once = [
        "xwaylandvideobridge"
        "waybar"
        "dunst"
        "nvibrant 0 0 0 400 0 400 0"
      ];

      # source = "./macchiato.conf";

      general = {
        gaps_in = "0";
        gaps_out = "0";

        border_size = "1";

        # https://wiki.hypr.land/Configuring/Variables/#variable-types for info about colors
        "col.active_border" = colors.pink;
        "col.inactive_border" = colors.surface1;

        # Set to true enable resizing windows by clicking and dragging on borders and gaps
        resize_on_border = false;

        # Please see https://wiki.hypr.land/Configuring/Tearing/ before you turn this on
        allow_tearing = false;

        layout = "dwindle";
      };

      decoration = {
        # rounding = "10";
        # rounding_power = "5";
        rounding = "0";
        rounding_power = "0";

        # https://wiki.hypr.land/Configuring/Variables/#blur
        blur = {
          size = "8";
          passes = "2";
        };

        # Change transparency of focused and unfocused windows
        active_opacity = "0.8";
        inactive_opacity = "0.6";

        shadow = {
          enabled = true;
          range = "4";
          render_power = "3";
          color = colors.pink;
          color_inactive = "0xff${colors.baseAlpha}";
        };
      };

      group = {
        "col.border_active" = colors.pink;
        "col.border_inactive" = "0xff${colors.baseAlpha}";

        groupbar = {
          "col.active" = colors.sapphire;
          "col.inactive" = "0xff${colors.baseAlpha}";
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
        smart_split = false;
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

        repeat_delay = "300";
        repeat_rate = "40";
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
        "$mainMod, RETURN, exec, ghostty"
        "$mainMod, C, killactive,"
        "$mainMod, D, exec, rofi -show drun"
        # "$mainMod, O, exit,"
        "$mainMod, E, exec, dolphin"
        "$mainMod, V, togglefloating,"
        "$mainMod, P, pseudo," # dwindle
        "$mainMod SHIFT, j, togglesplit," # dwindle
        "$mainMod, ESCAPE, exec, wlogout,"
        "$mainMod, F, fullscreen"

        # Screenshot a window
        # "SUPER_SHIFT, S, hyprshot -m window"
        # Screenshot a monitor
        # "SUPER_SHIFT, , exec, hyprshot -m output"
        # Screenshot a region
        "SUPER_SHIFT, S, exec, hyprshot -m region"

        # NOTE: group bindings (like the cute little tabs in sway)
        "$mainMod, T, togglegroup,"
        # "$mainMod SHIFT, T, changegroupactive, f,"
        "ALT, TAB, changegroupactive, f,"
        "SUPER_SHIFT, left, movewindoworgroup, l"
        "SUPER_SHIFT, right, movewindoworgroup, r"
        "SUPER_SHIFT, up, movewindoworgroup, u"
        "SUPER_SHIFT, down, movewindoworgroup, d"

        "$mainMod, h, movefocus, l"
        "$mainMod, l, movefocus, r"
        "$mainMod, k, movefocus, u"
        "$mainMod, j, movefocus, d"

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

        "$mainMod, S, togglespecialworkspace, magic"
        # "$mainMod SHIFT, S, movetoworkspace, special:magic"

        "$mainMod, mouse_down, workspace, e+1"
        "$mainMod, mouse_up, workspace, e-1"

        # TODO: resize window binds (width mainly) resizeactive x y
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
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [xdg-desktop-portal-gtk];
    config = {
      hyprland = {
        default = ["hyprland" "gtk"];
        # "org.freedesktop.impl.portal.FileChooser" = ["kde"];
        # "org.freedesktop.impl.portal.FileChooser" = "kde";
        "org.freedesktop.portal.Settings" = "gtk";
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
    enable = true;
  };

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
