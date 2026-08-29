{
  inputs,
  pkgs,
  ...
}: {
  imports = [inputs.noctalia.homeModules.default];

  programs.noctalia = {
    enable = true;
    package = inputs.noctalia.packages.${pkgs.system}.default;
    settings = {
      plugins.colorSchemes = {
        predefinedScheme = "Catppuccin";
        darkMode = true;
      };
      bar = {
        position = "top";
      };
    };
  };
}
