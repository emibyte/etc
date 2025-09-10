{
  pkgs,
  inputs,
  lib,
  ...
}: {
  # NOTE: automatically also installs spotify
  programs.spicetify = let
    spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.stdenv.system};
  in {
    enable = true;
    theme = lib.mkForce spicePkgs.themes.catppuccin;
    colorScheme = lib.mkForce "mocha";
  };
}
