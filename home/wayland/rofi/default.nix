{
  pkgs,
  config,
  ...
}: {
  programs.rofi = {
    enable = true;

    package = pkgs.rofi-wayland.override {plugins = [pkgs.rofi-emoji];};
    font = "Fira Code 16";
    extraConfig = {
      case-sensitive = false;
      display-drun = "Apps:";
      modi = ["drun" "run" "emoji"];
      show-icons = true;
    };
    pass = {
      enable = true;
      package = pkgs.rofi-pass-wayland;
      stores = ["~/.pass"];
    };
    plugins = with pkgs; [
      rofi-bluetooth
      rofi-pulse-select
    ];
    theme = ./catppuccin-frappe.rasi;

    /*
    theme = let
      mkLiteral = config.lib.formats.rasi.mkLiteral;
    in {
      "*" = {
        bg = mkLiteral "#303446FF";
        fg = mkLiteral "#D9E0EEFF";
        ac = mkLiteral "#8CAAEEFF";
      };

      "#window" = {
        location = mkLiteral "center";
        width = mkLiteral "50%";
      };

      "#prompt" = {
        background-color = mkLiteral "@bg";
        text-color = mkLiteral "@fg";
      };

      "#textbox-prompt-colon" = {
        text-color = mkLiteral "@fg";
      };

      "#entry" = {
        background-color = mkLiteral "@bg";
        text-color = mkLiteral "@fg";
        blink = mkLiteral "true";
      };

      "#inputbar" = {
        children = mkLiteral "[ prompt, entry ]";
        background-color = mkLiteral "@bg";
        text-color = mkLiteral "@fg";
        padding = mkLiteral "5px";
      };

      "#listview" = {
        background-color = mkLiteral "@bg";
        columns = mkLiteral "1";
        lines = mkLiteral "5";
        cycle = mkLiteral "false";
        dynamic = mkLiteral "true";
      };

      "#mainbox" = {
        background-color = mkLiteral "@bg";
        border = mkLiteral "3px";
        border-color = mkLiteral "@ac";
        children = mkLiteral "[ inputbar, listview ]";
        padding = mkLiteral "10px";
      };

      "#element" = {
        background-color = mkLiteral "@bg";
        text-color = mkLiteral "@fg";
        padding = mkLiteral "5px";
      };

      "#element-icon" = {
        background-color = mkLiteral "@bg";
        text-color = mkLiteral "@fg";
        size = mkLiteral "32px";
      };

      "#element-text" = {
        background-color = mkLiteral "@bg";
        text-color = mkLiteral "@fg";
        padding = mkLiteral "5px";
      };

      "#element selected" = {
        border = mkLiteral "3px";
        border-color = mkLiteral "@ac";
      };
    };
    */
  };
}
