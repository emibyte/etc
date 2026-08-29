{pkgs, ...}: {
  stylix = {
    enable = true;
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
    targets = {
      fontconfig.enable = false;
      chromium.enable = false;
    };

    fonts = {
      monospace = {
        package = pkgs.maple-mono.NF-unhinted;
        name = "Maple Mono NF";
      };
    };

    icons = {
      enable = true;
      package = pkgs.catppuccin-papirus-folders.override {
        flavor = "mocha";
        accent = "pink";
      };
      dark = "Papirus-Dark";
    };
    # NOTE: try sakura.yaml, stella and cupcake
    #       https://tinted-theming.github.io/tinted-gallery/
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/horizon-light.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/unikitty-light.yaml";
    # NOTE: this is supposed to be the hot pink color theme from kde
    # base16Scheme = {
    #   base00 = "f8bbd0";
    #   base01 = "fce4ec";
    #   base02 = "efefef";
    #   base03 = "7f8c8d";
    #   base04 = "dadeda";
    #   base05 = "31363b";
    #   base06 = "eeeeee";
    #   base07 = "fcfcfc";
    #   base08 = "da4453";
    #   base09 = "f67400";
    #   base0A = "c9ce3b";
    #   base0B = "27ae60";
    #   base0C = "11d116";
    #   base0D = "2980b9";
    #   base0E = "e91e63";
    #   base0F = "ab47bc";
    # };
  };
}
