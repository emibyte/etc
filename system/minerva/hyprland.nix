{pkgs, ...}: {
  programs.hyprland.enable = true;
  environment.systemPackages = with pkgs; [
    kdePackages.xwaylandvideobridge
    playerctl
    pavucontrol
    pamixer
    wl-screenrec
    wl-clipboard
    wl-clip-persist
    wlogout
  ];
}
