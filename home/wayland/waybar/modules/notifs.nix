{ pkgs, ... }: 
let
  dunstctl = "${pkgs.dunst}/bin/dunstctl";
in {
  interval = 1;
  tooltip = false;
  return-type = "json";

  on-click = "${dunstctl} set-paused toggle";

  exec = pkgs.writeShellScript "dunst-waybar" ''
    if [ "$(${dunstctl} is-paused)" == "true" ]; then
        echo '{"class": "off", "text":  " "}'
    else
        echo '{"class": "on", "text": " "}'
    fi
  '';
}
