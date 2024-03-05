{ pkgs, ... }:
{
  home.file."wezterm" = {
    source = ./wezterm;
    target = ".config/wezterm";
    recursive = true;
  };
}
