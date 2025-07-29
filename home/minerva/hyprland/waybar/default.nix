{pkgs, ...}: let
  powermenu = pkgs.writeShellScript "powermenu" ''
    logout="⇠  Log Out"
    shutdown="  Shut Down"
    reboot="  Reboot"
    sleep="  Sleep"
    hibernate="  Hibernate"
    # Get answer from user via rofi
    selected_option=$(echo "$lock
    $logout
    $sleep
    $hibernate
    $reboot
    $shutdown" | rofi -dmenu\
                      -i\
                      -p "Power")
    # Do something based on selected option
    if [ "$selected_option" == "$logout" ]
    then
        hyprctl dispatch exit 0
    elif [ "$selected_option" == "$shutdown" ]
    then
        systemctl poweroff
    elif [ "$selected_option" == "$reboot" ]
    then
        systemctl reboot
    elif [ "$selected_option" == "$sleep" ]
    then
        systemctl suspend
    elif [ "$selected_option" == "$hibernate" ]
    then
        systemctl hibernate
    else
        echo "No match"
    fi
  '';
in {
  # TODO: make a notification widget, that i can enable/disable notifications
  #       but also can be opened to show a list of past notifications
  xdg.userDirs.enable = true;
  services.mpd.enable = true;
  services.mpd-mpris.enable = true;
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        passthrough = false;
        gtk-layer-shell = true;
        margin-left = 6;
        margin-right = 6;
        margin-top = 2;

        modules-left = [
          "idle_inhibitor"
          "temperature"
          "cpu"
          "memory"
          "disk"
          "hyprland/workspaces#kanji"
          "tray"
          "mpris"
        ];
        modules-center = [
          "custom/swaync"
        ];
        modules-right = [
          "hyprland/window"
          "pulseaudio"
          "network"
          "custom/power"
        ];

        "custom/power" = {
          tooltip = false;
          on-click = "exec ${powermenu}";
          format = "";
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-bluetooth = "{volume}% {icon}";
          format-muted = "󰖁 muted";
          format-icons = {
            headphone = "";
            hands-free = "󰋎";
            headset = "󰋎";
            default = "󰕾";
          };

          # Interaction
          scroll-step = 5;
          on-click = "pactl set-sink-mute @DEFAULT_SINK@ toggle";
          on-click-right = "pavucontrol";
        };

        # TODO: idk
        "pulseaudio#microphone" = {
          format = "{format_source}";
          format-source = " {volume}%";
          format-source-muted = "";
          on-click = "";
          on-click-right = "pavucontrol -t 4";
          on-scroll-up = "";
          on-scroll-down = "";
          tooltip-format = "{source_desc} | {source_volume}%";
          scroll-step = 5;
        };

        network = {
          format = "{ifname}";
          format-wifi = "{icon}";
          format-ethernet = "󰌘";
          format-disconnected = "󰌙";
          tooltip-format = "{ipaddr}  {bandwidthUpBits}  {bandwidthDownBits}";
          format-linked = "󰈁 {ifname} (No IP)";
          tooltip-format-wifi = "{essid} {icon} {signalStrength}%";
          tooltip-format-ethernet = "{ifname} 󰌘";
          tooltip-format-disconnected = "󰌙 Disconnected";
          max-length = 30;
          format-icons = [
            "󰤯"
            "󰤟"
            "󰤢"
            "󰤥"
            "󰤨"
          ];
          # on-click-right= "$HOME/.config/hypr/scripts/WaybarScripts.sh --nmtui";
        };

        idle_inhibitor = {
          tooltip = true;
          tooltip-format-activated = "Idle_inhibitor active";
          tooltip-format-deactivated = "Idle_inhibitor not active";
          format = "{icon}";
          format-icons = {
            activated = " ";
            deactivated = " ";
          };
        };

        memory = {
          interval = 10;
          format = "{used:0.1f}G 󰾆";
          format-alt = "{percentage}% 󰾆";
          format-alt-click = "click";
          tooltip = true;
          tooltip-format = "{used:0.1f}GB/{total:0.1f}G";
          # on-click-right = ""; # TODO: open ghostty in floating with btop
        };

        temperature = {
          interval = 10;
          tooltip = true;
          hwmon-path = [
            "/sys/class/hwmon/hwmon1/temp1_input"
            "/sys/class/thermal/thermal_zone0/temp"
          ];
          # thermal-zone = 0;
          critical-threshold = 90;
          format-critical = "{temperatureC}°C {icon}";
          format = "{temperatureC}°C {icon}";
          format-icons = [
            "󰈸"
          ];
          # on-click-right = ""; # TODO: nvtop maybe?
        };

        bluetooth = {
          format = " ";
          format-disabled = "󰂳";
          format-connected = "󰂱 {num_connections}";
          tooltip-format = " {device_alias}";
          tooltip-format-connected = "{device_enumerate}";
          tooltip-format-enumerate-connected = " {device_alias} 󰂄{device_battery_percentage}%";
          tooltip = true;
          on-click = "blueman-manager";
        };

        cpu = {
          format = "{usage}% 󰍛";
          interval = 1;
          min-length = 5;
          format-alt-click = "click";
          format-alt = "{icon0}{icon1}{icon2}{icon3} {usage:>2}% 󰍛";
          format-icons = [
            "▁"
            "▂"
            "▃"
            "▄"
            "▅"
            "▆"
            "▇"
            "█"
          ];
          # on-click-right = "gnome-system-monitor";
        };

        disk = {
          interval = 30;
          path = "/";
          format = "{percentage_used}% 󰋊";
          tooltip = true;
          tooltip-format = "{used} used out of {total} on {path} ({percentage_used}%)";
        };

        tray = {
          icon-size = 20;
          spacing = 4;
        };

        mpris = {
          interval = 10;
          format = "{player_icon} ";
          format-paused = "{status_icon} <i>{dynamic}</i>";
          on-click-middle = "playerctl play-pause";
          on-click = "playerctl previous";
          on-click-right = "playerctl next";
          scroll-step = 5.0;
          # on-scroll-up = "$HOME/.config/hypr/scripts/Volume.sh --inc";
          # on-scroll-down = "$HOME/.config/hypr/scripts/Volume.sh --dec";
          smooth-scrolling-threshold = 1;
          tooltip = true;
          tooltip-format = "{status_icon} {dynamic}\nLeft Click: previous\nMid Click: Pause\nRight Click: Next";
          player-icons = {
            "chromium" = "";
            "default" = "";
            "firefox" = "";
            "kdeconnect" = "";
            "mopidy" = "";
            "mpv" = "󰐹";
            "spotify" = "";
            "vlc" = "󰕼";
          };
          status-icons = {
            "paused" = "󰐎";
            "playing" = "";
            "stopped" = "";
          };
          "max-length" = 30;
        };

        "custom/swaync" = {
          tooltip = true;
          tooltip-format = "Left Click: Launch Notification Center\nRight Click: Do not Disturb";
          format = "{} {icon} ";
          format-icons = {
            notification = "<span foreground='red'><sup></sup></span>";
            none = "";
            dnd-notification = "<span foreground='red'><sup></sup></span>";
            dnd-none = "";
            inhibited-notification = "<span foreground='red'><sup></sup></span>";
            inhibited-none = "";
            dnd-inhibited-notification = "<span foreground='red'><sup></sup></span>";
            dnd-inhibited-none = "";
          };
          return-type = "json";
          exec-if = "which swaync-client";
          exec = "swaync-client -swb";
          on-click = "sleep 0.1 && swaync-client -t -sw";
          on-click-right = "swaync-client -d -sw";
          escape = true;
        };

        "clock#2" = {
          "format" = "  {:%H:%M}";
          "format-alt" = "{:%A  |  %H:%M  |  %e %B}";
          "tooltip-format" = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };

        "hyprland/window" = {
          format = "{}";
          max-length = 25;
          separate-outputs = true;
          offscreen-css = true;
          offscreen-css-text = "(inactive)";
          rewrite = {
            "(.*) — Mozilla Firefox" = " $1";
            "(.*) - fish" = "> [$1]";
            "(.*) - zsh" = "> [$1]";
            "(.*) - ghostty" = "> [$1]";
          };
        };

        "hyprland/workspaces#kanji" = {
          disable-scroll = true;
          show-special = false;
          all-outputs = true;
          format = "{icon}";
          persistent-workspaces = {
            "*" = 5;
          };
          format-icons = {
            "1" = "一";
            "2" = "二";
            "3" = "三";
            "4" = "四";
            "5" = "五";
            "6" = "六";
            "7" = "七";
            "8" = "八";
            "9" = "九";
            "10" = "十";
          };
        };

        style = builtins.readFile ./style.css;
      };
      xdg.configFile."waybar/mocha.css".text =
        builtins.readFile ./mocha.css;
    };
  };
}
