{pkgs, config, ...}: {
  imports = [./bindings.nix];

  wayland.windowManager.sway = {
    enable = true;
    config = {
      modifier = "Mod4";
      terminal = "ghostty";
      menu = "rofi -show run";
      bars = [{command = "${pkgs.waybar}/bin/waybar";}];
      input = {
        "type:keyboard" = {
          xkb_layout = "de";
          # xkb_options = "caps:swapescape";
          xkb_options = "caps:ctrl_modifier";
          xkb_numlock = "enabled";

          repeat_delay = "300";
          repeat_rate = "50";
        };
      };

      startup = [
        {command = "systemctl --user import-environment";}
        {command = "${pkgs.autotiling}/bin/autotiling";}
        {command = "ckb-next --background";}
        {
          # TODO: absolute path requires --impure to nix build replace with {config.homeDirectory} like in rebuild-system command
          command = let
            setWallpaper = pkgs.writeShellScript "set-wallpaper" ''
              ${pkgs.killall}/bin/killall swaybg
              ${pkgs.swaybg}/bin/swaybg -m fill -i ${config.home.homeDirectory}/wps/wallhaven-85prg1_2560x1600.png
            '';
          in "${setWallpaper}";
          always = true;
        }
        {
          command = "${pkgs.kanshi}/bin/kanshi";
          always = true;
        }
      ];
      window.border = 3;
      window.titlebar = false;
      gaps.inner = 10;
    };
    extraConfig = ''
      for_window [class=".*"] opacity 0.9
      for_window [app_id=".*"] opacity 0.9
      client.focused #FFADAD #FFADAD #FFADAD #FFADAD
    '';
  };
}
