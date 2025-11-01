{pkgs, ...}: {
  programs.mangohud = {
    enable = true;
    package = pkgs.mangohud;
    settings = {
      preset = 3;
      # legacy_layout = false;
      # gpu_stats = true;
      # gpu_temp = true;
      # cpu_stats = true;
      # cpu_temp = true;
      # ram = true;
      # fps = true;
      # frame_timing = true;
      # frame_count = true;
      # toggle_logging = "Shift_L+F2";
      # toggle_hud_position = "Shift_R+F11";
      # fps_limit_method = "late";
      # fps_limit = 0;
      # toggle_fps_limit = "Shift_L+F1";
      # # background_alpha = "0.5";
      # position = "top-left";
      # toggle_hud = "Shift_R+F12";
      # # gpu_color = "2e9762";
      # # cpu_color = "2e97cb";
      # fps_value = "60,144";
      # # fps_color = "cc0000,ffaa7f,92e79a";
      # gpu_load_value = "60,90";
      # # gpu_load_color = "92e79a,ffaa7f,cc0000";
      # cpu_load_value = "60,90";
      # # cpu_load_color = "92e79a,ffaa7f,cc0000";
      # # background_color = "000000";
      # # frametime_color = "00ff00";
      # # vram_color = "ad64c1";
      # # ram_color  = "c26693";
      # # wine_color = "eb5b5b";
      # # engine_color = "eb5b5b";
      # # text_color = "ffffff";
      # # media_player_color = "ffffff";
      # # network_color = "e07b85";
      # # battery_color = "92e79a";
      # media_player_format = "{title};{artist};{album}";
    };
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
      pavucontrol = {
        no_display = true;
      };
      veadotube-mini = {
        no_display = true;
      };
    };
  };
}
