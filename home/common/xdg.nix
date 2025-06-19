{pkgs, ...}: let
  setVibrance = pkgs.writeText "nvibrant.desktop" ''
    [Desktop Entry]
    Type=Application
    Name=nVibrant
    Exec=nvibrant 0 0 0 400 0 400 0
    X-GNOME-Autostart-enabled=true
    NoDisplay = true
  '';
in {
  xdg = {
    enable = true;
    autostart = {
      enable = true;
      entries = [setVibrance];
    };
  };
}
