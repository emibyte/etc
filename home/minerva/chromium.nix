{pkgs, ...}: {
  programs.chromium = {
    enable = true;
    commandLineArgs = [
      "--ozone-platform=wayland"
      "--ozone-platform-hint=auto"
    ];
  };
}
