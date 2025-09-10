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
  home.packages = with pkgs; [blueberry morgen];
  xdg.userDirs.enable = true;
  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        passthrough = false;
        spacing = 0;
        margin-left = 6;
        margin-right = 6;
        margin-top = 2;

        modules-left = [
          "hyprland/workspaces"
          "cpu"
        ];
        modules-center = [
          "hyprland/window"
        ];
        modules-right = [
          "tray"
          "clock"
          "clock#date"
          "bluetooth"
          "network"
          "pulseaudio"
          "custom/power"
        ];

        "custom/power" = {
          tooltip = false;
          on-click = "exec ${powermenu}";
          format = "";
        };

        tray = {
          icon-size = 19;
          spacing = 10;
        };

        "hyprland/window" = {
          separate-outputs = true;
        };

        "hyprland/workspaces" = {
          format = "{name}: {icon}";
          format-icons = {
            active = "";
            default = "";
          };
        };

        bluetooth = {
          format = "󰂲";
          format-on = "{icon}";
          format-off = "{icon}";
          format-connected = "{icon}";
          format-icons = {
            on = "󰂯";
            off = "󰂲";
            connected = "󰂱";
          };
          on-click = "blueman-manager";
          tooltip-format-connected = "{device_enumerate}";
        };

        "custom/music" = {
          format = "  {}";
          escape = true;
          interval = 5;
          tooltip = false;
          exec = "playerctl metadata --format='{{ artist }} - {{ title }}'";
          on-click = "playerctl play-pause";
          max-length = 50;
        };

        clock = {
          timezone = "Europe/Berlin";
          tooltip = true;
          tooltip-format = "{:%H:%M:%S}";
          # format = "{:%H:%M:%S  -  %A; %d}";
          format = "  {:%H:%M}";
          interval = 1;
        };

        "clock#date" = {
          interval = 5;
          format = "  {:%d/%m/%Y}";
        };

        network = {
          format-wifi = "󰤢";
          format-ethernet = "{ifname} 󰈀";
          format-disconnected = "󰤠";
          interval = 5;
          tooltip-format = "{essid} ({signalStrength}%)";
          on-click = "nm-connection-editor";
        };

        cpu = {
          interval = 1;
          format = "  {icon0}{icon1}{icon2}{icon3} {usage:>2}%";
          format-icons = ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"];
          on-click = "ghostty -e htop";
        };

        memory = {
          interval = 30;
          format = "  {used:0.1f}G/{total:0.1f}G";
          tooltip-format = "Memory";
        };

        pulseaudio = {
          format = "{icon}  {volume}%";
          format-muted = "";
          format-icons = {
            default = ["" "" " "];
          };
          on-click = "pavucontrol";
        };
        "custom/lock" = {
          tooltip = false;
          on-click = "sh -c '(sleep 0s; hyprlock)' & disown";
          format = "";
        };

        "custom/pomodoro" = {
          format = "{}";
          return-type = "json";
          exec = "waybar-module-pomodoro --no-work-icons";
          on-click = "waybar-module-pomodoro toggle";
          on-click-right = "waybar-module-pomodoro reset";
        };
      };
    };
    style = builtins.readFile ./style.css;
  };
  xdg.configFile."waybar/mocha.css".text =
    builtins.readFile ./mocha.css;
}
