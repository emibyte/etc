{ pkgs, ... }: {
  imports = [
    ./sway
    ./waybar
    ./kanshi.nix
    ./swaylock.nix
    ./rofi.nix
  ];

  home.packages = with pkgs; [
    swaylock-fancy
    swayidle
    wl-clipboard
    mako
    wofi
    waybar
    swaybg

    wdisplays
    wlr-randr
  ];

  home.sessionVariables = {
    NIXOS_OZONE_WL = 1;

    SDL_VIDEODRIVER = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XDG_SESSION_TYPE = "wayland";
  };

}