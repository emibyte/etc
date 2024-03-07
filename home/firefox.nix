{ ... }: {
  programs.firefox = {
    enable = true;

    profiles = {
      emi= {
        isDefault = true;
        search = {
          default = "DuckDuckGo";
          force = true;
          privateDefault = "DuckDuckGo";
        };
        settings = {
            "browser.aboutConfig.showWarning" = false;
            "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.feeds.snippets" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.showSearch" = false;
        };
      };
    };
  };
}
