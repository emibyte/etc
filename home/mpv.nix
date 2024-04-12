{...}: {
  programs.mpv.enable = true;
  programs.mpv.config = {
    fs = "yes";
    volume-max = 300;
  };
}
