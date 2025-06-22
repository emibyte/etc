{lib, ...}: {
  programs = {
    # configuration only, setting it as login shell has
    # to happen on system level (configuration.nix)
    zsh = {
      enable = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;

      oh-my-zsh = {
        enable = true;
        plugins = ["git"];
      };
    };

    bash = {
      enable = true;
      enableCompletion = true;
    };

    starship = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      settings = {
        format = lib.concatStrings [
          "$all"
        ];
        # TODO: write a nix function that does this for any given username
        username = {
          show_always = true;
          # style_user = "bold green";
          # Default format
          # format = "[$user]($style)@";
          # Vibrant Spectrum
          # format = "[e](bold #FF5733)[m](bold #FFBD33)[i](bold #75FF33)[l](bold #33FFBD)[y](bold #335BFF)@";
          # Sunset Rainbow
          # format =  "[e](bold #FF6F61)[m](bold #FF9A00)[i](bold #FFD700)[l](bold #A3D9FF)[y](bold #6A5ACD)@";
          # Bold and Bright
          format = "[e](bold #FF3D00)[m](bold #FFEA00)[i](bold #00E676)[l](bold #00B0FF)[y](bold #6200EA)@";
        };
        hostname = {
          ssh_only = false;
          format = "[$ssh_symbol$hostname]($style): ";
          style = "bold #335BFF";
        };
        character = {
          success_symbol = "[λ](bold #ff8bc1)";
          error_symbol = "[λ](bold red)";
          # success_symbol = "[➜](bold green)";
          # error_symbol = "[➜](bold red)";
        };

        nix_shell = {
          format = "[$symbol $state]($style) ";
          symbol = "❄️";
        };
      };
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    fzf.enable = true;
  };
}
