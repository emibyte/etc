{...}: {
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
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
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
