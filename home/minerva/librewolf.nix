{pkgs, ...}: {
  programs.librewolf = {
    enable = true;
    package = pkgs.librewolf-bin;
  };
}
