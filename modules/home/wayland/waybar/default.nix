{pkgs, ...}: {
  programs.waybar = {
    enable = true;

    # systemd.enable = true;

    settings = [
      (
        {
          layer = "top";
          position = "top";
          spacing = 0;
          height = 34;

          fixed-center = false;

          modules-left = ["sway/workspaces"];
          modules-center = ["custom/spotify"];
          modules-right = [
            "tray"
            "memory"
            "pulseaudio"
            "backlight"
            "battery"
            "network"
            "clock"
            "clock#date"

          ];
        }
        // (import ./modules pkgs)
      )
    ];
    style = builtins.readFile ./style.css;
  };
  xdg.configFile."waybar/mocha.css".text =
    builtins.readFile ./mocha.css;
}
