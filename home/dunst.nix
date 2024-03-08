{
  services.dunst = {
    enable = true;

    settings = {
      global = {
        width = "(0, 800)";
        height = 300;
        offset = "10x50";
        horizontal_padding = 10;
        frame_color = "#FFADAD";
        font = "JetBrains Mono 16";
        format = "%s %p\\n%b";

        background = "#282828";
        foreground = "#ffffff";
        timeout = 2;

      };

      urgency_low = {};

      urgency_normal = {};

      urgency_critical = {
        frame_color = "#cc241d";
        timeout = 0;
      };
    };
  };
}
