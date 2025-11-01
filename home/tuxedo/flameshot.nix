{config, ...}: {
  services.flameshot = {
    enable = true;

    settings = {
      General = {
        drawColor = "#ff0000";
        drawFontSize = 23;
        drawThickness = 3;

        # TODO: absolute path requires --impure to nix build replace with {config.homeDirectory} like in rebuild-system command
        savePath = "${config.home.homeDirectory}/tmp/";
        savePathFixed = false;

        disabledTrayIcon = true;
        showDesktopNotification = false;
        showHelp = false;

        uiColor = "#ffffff";
      };
    };
  };
}
