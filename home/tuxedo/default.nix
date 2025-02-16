
{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./nvim
    ./themes
    ./wayland
    ./dunst.nix
    ./flameshot.nix
    ./gammastep.nix
    ./imv.nix
  ];
  home.packages = [
    pkgs.autotiling
  ];
}
