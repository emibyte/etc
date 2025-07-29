{pkgs, ...}: {
  home.packages = with pkgs; [
    (catppuccin-kde.override {
      flavour = ["latte"];
      accents = ["pink"];
      winDecStyles = ["classic"];
    })
    (catppuccin-kvantum.override {
      variant = "latte";
      accent = "pink";
    })
  ];
  catppuccin = {
    enable = false;
    flavor = "latte";
  };

  # qt = {
  #   enable = true;
  #   platformTheme.name = "kvantum";
  #   style.name = "kvantum";
  # };
}
