{pkgs, ...}: {
  imports = [
    ./librewolf.nix
    ./chromium.nix
    ./fhs.nix
    ./hyprland
  ];

  home.packages = [
    pkgs.nvibrant-git

    pkgs.xivlauncher
    pkgs.nvtopPackages.full
    pkgs.deadlock-mod-manager

    pkgs.pokemmo-installer

  ];
}
