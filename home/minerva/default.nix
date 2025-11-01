{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
    ./fhs.nix
    ./hyprland
  ];

  home.packages = with pkgs; [
    xivlauncher
    nvtopPackages.full
  ];
}
