{
  lib,
  config,
  ...
}: {
  programs = {
    # configuration only, setting it as login shell has
    # to happen on system level (configuration.nix)
    zsh = {
      enable = true;

      # NOTE(emi): 26.05 use new default
      dotDir = "${config.xdg.configHome}/zsh";

      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      enableCompletion = true;
      envExtra = ''
        export PATH="$HOME/go/bin:$PATH"
      '';

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
        username = {
          show_always = true;
          format = "[e](bold #FF3D00)[m](bold #FFEA00)[i](bold #00E676)[l](bold #00B0FF)[y](bold #6200EA)::";
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

    bat.enable = true;

    zoxide = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
    };
  };
}
