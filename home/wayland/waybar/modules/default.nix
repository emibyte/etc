pkgs: {
  # LEFT
  "sway/workspaces" = {
    persistent-workspaces = {
      "1" = [];
      "2" = [];
      "3" = [];
      "4" = [];
      "5" = [];
      "6" = [];
      "7" = [];
      "8" = [];
      "9" = [];
      "10" = [];
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
    enable-bar-scroll = true;
    disable-scroll-wraparound = true;
  };

  # RIGHT
  backlight = {
    device = "intel_backlight";
    format = "{icon}";
    scroll-step = 10.0;
    format-icons = ["" "" "" "" "" "" "" "" ""];
    tooltip = true;
    tooltip-format = "{percent}%";
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

  network = {
    format-wifi = "";
    format-disconnected = "睊";
    tooltip = true;
    tooltip-format = "{essid} @ {signalStrength}%";

    format = "{ifname}";
    format-ethernet = "󰈀 {ipaddr}";
  };

  memory = {
    interval = 5;
    format = "󰍛 {}%";
    max-length = 10;
  };

  battery = {
    tooltip = false;

    bat = "BAT0";
    adapter = "AC0";

    format = "{icon}  {capacity}%";
    format-full = "{icon}  {capacity}% 󰚥";
    format-charging = "{icon}  {capacity}% 󰚥";

    format-icons = ["" "" "" "" ""];
    # format-icons = [ "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
  };

  clock = {
    interval = 5;
    format = "  {:%H:%M}";
  };

  "clock#date" = {
    interval = 5;
    format = "  {:%d/%m/%Y}";
  };

  "custom/notifs" = import ./notifs.nix pkgs;

  tray = {
    icon-size = 20;
    spacing = 10;
  };
}
