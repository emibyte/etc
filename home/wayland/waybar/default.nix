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
          # modules-center = ["sway/window"];
          modules-right = [
            "tray"
            "memory"
            "cpu"
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
  xdg.configFile."waybar/frappe.css".text =
    builtins.readFile ./frappe.css;
}
