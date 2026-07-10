{...}: {
  programs.mpv = {
    enable = true;
    config = {
      profile = "high-quality";
      vo = "gpu-next";
      gpu-api = "vulkan";
      hwdec = "yes";
      dither-depth = "auto";
      target-colorspace-hint = "yes";
      audio-exclusive = "no";
      audio-channels = "auto-safe";
      sub-fix-timing = "yes";
      # https://mpv.io/manual/master/#options-interpolation
      # Reduces stuttering in exchange for blurring in motion scenes ; remove the lines in case you don't want the trade-off
      video-sync = "display-resample";
      interpolation = "yes";

      fs = "yes";
      volume-max = 300;
    };
  };
}
