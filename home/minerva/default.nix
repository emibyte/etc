{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
    ./mangohud.nix
  ];

  home.packages = [
    pkgs.goverlay
  ];
}
