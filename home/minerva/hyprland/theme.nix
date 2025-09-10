{pkgs, ...}: {
  home.packages = with pkgs; [
    (catppuccin-kde.override {
      flavour = ["mocha"];
      accents = ["pink"];
      winDecStyles = ["classic"];
    })
    (catppuccin-kvantum.override {
      variant = "mocha";
      accent = "pink";
    })
  ];
  catppuccin = {
    enable = true;
    flavor = "mocha";
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style.name = "kvantum";
  };
}
