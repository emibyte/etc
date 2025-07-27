{
  pkgs,
  inputs,
  ...
}: {
  # NOTE: automatically also installs spotify
  programs.spicetify = let
    spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.stdenv.system};
  in {
    enable = true;
    theme = spicePkgs.themes.catppuccin;
    colorScheme = "latte";
  };
}
