{pkgs, ...}: {
  programs.hyprland.enable = true;
  services.blueman.enable = true;

  services.gnome.gnome-keyring.enable = true;
  security.pam.services.login.enableGnomeKeyring = true;
  security.pam.services.login.kwallet.enable = false;
  security.pam.services.kwallet.enable = false;

  security.polkit.enable = true; # NOTE: needed this for swaylock, not sure if it's necessary for hyprlock
  security.pam.services.hyprlock = {};

  programs.thunar = {
    enable = true;
    plugins = with pkgs.xfce; [
      thunar-archive-plugin
      thunar-media-tags-plugin
      thunar-volman
    ];
  };

  environment.systemPackages = with pkgs; [
    kdePackages.xwaylandvideobridge
    # libsForQt5.xwaylandvideobridge
    playerctl
    pavucontrol
    pamixer
    wl-screenrec
    wl-clipboard
    wl-clip-persist
    wlogout
  ];
}
