{pkgs, ...}: {
  home.packages = with pkgs; [
    (catppuccin-kde.override {
      flavour = ["macchiato"];
      accents = ["pink"];
      winDecStyles = ["classic"];
    })
    (catppuccin-kvantum.override {
      variant = "macchiato";
      accent = "pink";
    })
  ];
  catppuccin = {
    enable = true;
    flavor = "macchiato";
  };

  qt = {
    enable = true;
    platformTheme.name = "kvantum";
    style.name = "kvantum";
  };
}
