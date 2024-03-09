{pkgs, ...}: {

  programs.waybar = {
    enable = true;

    settings = [
      (
        {
          layer = "top";
          position = "top";
          spacing = 0;
          height = 34;

          fixed-center = false;

          modules-left = ["sway/workspaces"];
          # TODO: spotify
          # modules-center = ["custom/spotify"];
          modules-right = [
            "tray"
            "memory"
            "pulseaudio"
            "backlight"
            "battery"
            "network"
            "clock"
            "clock#date"
            "custom/notifs"
          ];
        }
        # NOTE: import cannot use variables that are in scope
        #       which means pkgs needs to be passed as an arg
        // (import ./modules pkgs)
      )
    ];
    style = builtins.readFile ./style.css;
  };
  xdg.configFile."waybar/mocha.css".text =
    builtins.readFile ./mocha.css;
}
