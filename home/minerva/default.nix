{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
    ./mangohud.nix
    ./fhs.nix
    # ./hyprland
  ];

  home.packages = with pkgs; [
    xivlauncher
    nvtopPackages.full
  ];
}
