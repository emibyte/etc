{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
  ];
}
