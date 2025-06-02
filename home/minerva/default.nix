{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
    ./mangohud.nix
  ];
}
