{pkgs, ...}: {
  programs.mangohud = {
    enable = true;
    package = pkgs.mangohud;
    enableSessionWide = true;
  };
}
