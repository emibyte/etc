{pkgs, ...}: {
  programs.mangohud = {
    enable = true;
    package = pkgs.mangohud;
    enableSessionWide = true;
    settingsPerApplication = {
      foliate = {
        no_display = true;
      };
      # NOTE: this is the acual thing that runs foliate yay i found it
      #       disables mangohud inside foliate
      gjs-console = {
        no_display = true;
      };
      mpv = {
        no_display = true;
      };
      veadotube-mini = {
        no_display = true;
      };
    };
  };
}
