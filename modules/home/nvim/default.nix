{ pkgs, ... }:
{
  home.file."nvim" = {
    source = ./nvim;
    target = ".config/nvim";
    recursive = true;
  };
}
