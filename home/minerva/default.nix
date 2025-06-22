{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
    ./mangohud.nix
    ./fhs.nix
  ];

  home.packages = with pkgs; [
    xivlauncher
    nvtopPackages.full
  ];
}
