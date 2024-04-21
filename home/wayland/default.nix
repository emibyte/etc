{pkgs, ...}: {
  imports = [
    ./sway
    ./waybar
    ./rofi
    ./kanshi.nix
    ./swaylock.nix
  ];

  home.packages = with pkgs; [
    swaylock-fancy
    swayidle
    wl-clipboard
    mako
    wofi
    waybar
    swaybg
    wtype

    wdisplays
    wlr-randr

    grim
  ];

  home.sessionVariables = {
    NIXOS_OZONE_WL = 1;

    SDL_VIDEODRIVER = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XDG_SESSION_TYPE = "wayland";

    WLR_NO_HARDWARE_CURSORS = 1;
  };
}
