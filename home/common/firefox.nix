{
  pkgs,
  config,
  ...
}: {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-bin;

    # NOTE(emi): 26.05 change default value to what it will be in the future
    configPath = "${config.xdg.configHome}/mozilla/firefox";

    profiles = {
      emi = {
        id = 0;
        isDefault = true;
        search = {
          default = "ddg";
          force = true;
          privateDefault = "ddg";
        };
        extensions.force = true;
        settings = {
          "browser.aboutConfig.showWarning" = false;
          "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
          "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
          "browser.newtabpage.activity-stream.feeds.snippets" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = false;
          "browser.newtabpage.activity-stream.showSearch" = false;
        };
      };
      default = {
        id = 1;
        extensions.force = true;
      };
    };
  };

  stylix.targets.firefox.profileNames = ["emi"];
  stylix.targets.firefox.colorTheme.enable = true;
}
