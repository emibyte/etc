{...}: {
  programs.floorp = {
    enable = true;

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
        search = {
          default = "ddg";
          force = true;
          privateDefault = "ddg";
        };
      };
    };
  };

  stylix.targets.floorp.profileNames = ["emi"];
  stylix.targets.floorp.colorTheme.enable = true;
}
