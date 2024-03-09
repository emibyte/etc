{...}: let
  name = "fevy";

  # custom colors
  foreground = "#C6D0F5"; # text
  background = "#303446"; # base
  regular0 = "#DFDFDF"; # papirus
  regular1 = "#E78284"; # red
  regular2 = "#A6D189"; # green
  regular3 = "#E5C890"; # yellow
  regular4 = "#8CAAEE"; # blue
  regular5 = "#F4B8E4"; # pink
  regular6 = "#81C8BE"; # teal
  regular7 = "#F2D5CF"; # rosewater
  regular8 = "#EEBEBE"; # flamingo
  regular9 = "#CA9EE6"; # mauve
  regular10 = "#EA999C"; # maroon
  regular11 = "#EF9F76"; # peach
  regular12 = "#81C8BE"; # teal
  regular13 = "#99D1DB"; # sky
  regular14 = "#85C1DC"; # sapphire
  regular15 = "#BABBF1"; # lavender
in {
  home-manager = {
    users.${name} = {
      programs = {
        waybar = {
          enable = true;
          settings = {
            mainBar = {
              layer = "bottom";
              position = "top";
              height = 24;
              spacing = 0;
              output = [
                "eDP-1"
                "HDMI-A-1"
              ];
              modules-left = [
                "sway/workspaces"
                "sway/mode"
              ];
              modules-center = [
                "clock"
              ];
              modules-right = [
                "bluetooth"
                "network"
                "pulseaudio"
                "backlight"
                "cpu"
                "memory"
                "battery"
                "tray"
              ];
              "keyboard-state" = {
                numlock = true;
                capslock = true;
                format = "{name} {icon}";
                format-icons = {
                  locked = "";
                  unlocked = "";
                };
              };
              "sway/mode" = {
                format = "<span style=\"italic\">{}</span>";
              };
              "sway/workspaces" = {
                on-click = "activate";
                sort-by-number = true;
                format = "{icon}";
                format-icons = {
                  "1" = "";
                  "2" = "";
                  "3" = "";
                  "4" = "";
                  "5" = "";
                  "6" = "";
                  "7" = "";
                  "8" = "";
                  "9" = "";
                };
              };
              "sway/scratchpad" = {
                format = "{icon} {count}";
                show-empty = false;
                format-icons = [
                  ""
                  ""
                ];
                tooltip = true;
                tooltip-format = "{app}: {title}";
              };
              "sway/window" = {
                max-length = 30;
              };
              "bluetooth" = {
                format = "{status} ";
                format-disabled = "";
                format-connected = "{num_connections} connected ";
                tooltip-format = "{controller_alias}\t{controller_address}";
                tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{device_enumerate}";
                tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
              };
              "mpd" = {
                format = "{stateIcon} ({elapsedTime:%M:%S}/{totalTime:%M:%S})";
                format-disconnected = "Disconnected ";
                format-stopped = "{randomIcon}Stopped ";
                unknown-tag = "N/A";
                interval = 2;
                consume-icons = {
                  "on" = " ";
                };
                random-icons = {
                  off = "<span color=\"#f53c3c\"></span> ";
                  on = " ";
                };
                repeat-icons = {
                  on = "";
                };
                single-icons = {
                  on = " ";
                };
                state-icons = {
                  paused = "";
                  playing = "";
                };
                tooltip-format = "MPD (connected)";
                tooltip-format-disconnected = "MPD (disconnected)";
              };
              "tray" = {
                icon-size = 13;
                spacing = 8;
              };
              "clock" = {
                interval = 60;
                tooltip = false;
                format = "{:%A ● %d-%m-%Y ● %H:%M}";
              };
              "cpu" = {
                format = "{usage}% ";
                tooltip = false;
              };
              "memory" = {
                format = "{}% ";
              };
              "temperature" = {
                # thermal-zone" = 2;
                # hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
                critical-threshold = 80;
                # format-critical = "{temperatureC}°C {icon}";
                format = "{temperatureC}°C {icon}";
                format-icons = [
                  ""
                  ""
                  ""
                  ""
                ];
              };
              "backlight" = {
                # device = "acpi_video1";
                format = "{percent}% {icon}";
                format-icons = [
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                  ""
                ];
              };
              "battery" = {
                states = {
                  good = 95;
                  warning = 30;
                  critical = 15;
                };
                interval = 2;
                format = "{capacity}% {icon}";
                format-charging = "{capacity}% ";
                format-plugged = "{capacity}% ";
                format-alt = "{time} {icon}";
                # "format-good": ""; # An empty format will hide the module
                # "format-full": "";
                format-icons = [
                  ""
                  ""
                  ""
                  ""
                  ""
                ];
              };
              "network" = {
                # "interface" = "wlp2*"; # (Optional) To force the use of this interface
                format-wifi = "{signalStrength}% ";
                format-ethernet = "{ipaddr}/{cidr} ";
                tooltip-format = "{ifname} via {gwaddr} ";
                format-linked = "{ifname} (No IP) ";
                format-disconnected = "Disconnected ";
                format-alt = "{ifname} = {ipaddr}/{cidr} ";
              };
              "pulseaudio" = {
                # "scroll-step" = 1; # %; can be a float
                format = "{volume}% {icon} {format_source}";
                format-bluetooth = "{volume}% {icon} {format_source}";
                format-bluetooth-muted = " {icon} {format_source}";
                format-muted = "{volume}%  {format_source}";
                format-source = "{volume}% ";
                format-source-muted = "{volume}% ";
                format-icons = {
                  headphone = "";
                  hands-free = "";
                  headset = "";
                  phone = "";
                  portable = "";
                  car = "";
                  default = [
                    ""
                    ""
                    ""
                  ];
                };
              };
            };
          };
        };
      };
    };
  };
}
