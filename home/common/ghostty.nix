{pkgs, ...}: {
  programs.ghostty = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
      # font-family = "Iosevka Comfy";
      font-family = "Hack";
      font-size = 10;
      theme = "ef-dark";
      cursor-style = "block";
      shell-integration-features = "no-cursor";
    };
    themes = {
      ef-dark= {
        background = "000000";
        cursor-color = "ff76ff";
        foreground = "d0d0d0";
        palette = [
          "0=#1a1a1a"
          "1=#ef6560"
          "2=#0faa26"
          "3=#bf9032"
          "4=#3f95f6"
          "5=#d369af"
          "6=#4fbaef"
          "7=#d0d0d0"
          "8=#4b4b4b"
          "9=#ff5a7a"
          "10=#00a692"
          "11=#df8a5a"
          "12=#029fff"
          "13=#af85ff"
          "14=#1dbfcf"
          "15=#857f8f"
        ];
        selection-background = "2a234a";
        selection-foreground = "d0d0d0";
      };
    };
  };
}
