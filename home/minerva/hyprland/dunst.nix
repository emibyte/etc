{pkgs, ...}: {
  # home.packages = with pkgs; [dunst];
  services.dunst = {
    enable = true;
    settings = {
      global = {
        frame_color = "#cad3f5";
        separator_color = "frame";
        font = "JetBrains Mono 13";
        corner_radius = 10;
        offset = "5x5";
        origin = "bottom-right";
        notification-limit = 8;
        gap_size = 7;
        frame_width = 2;
        width = 400;
        height = 80;
        follow = "keyboard";
        # format = "%s %p\\n%b";
      };

      urgency_low = {
        background = "#24273A";
        foreground = "#CAD3F5";
      };

      urgency_normal = {
        background = "#24273A";
        foreground = "#CAD3F5";
      };

      urgency_critical = {
        background = "#24273A";
        foreground = "#CAD3F5";
        frame_color = "#F5A97F";
      };
    };
  };
}
