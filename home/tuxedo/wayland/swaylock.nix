{pkgs, ...}: {
  programs.swaylock = {
    enable = true;
    # NOTE: this doesnt seem to do anything
    #       i'd liek to use swaylock-effects bcs of the --clock attribute
    package = pkgs.swaylock-effects;
    settings = {
      daemonize = true;

      # TODO: absolute path requires --impure to nix build replace with {config.homeDirectory} like in rebuild-system command
      image = builtins.toString /home/emily/wps/wallhaven-wewl9r_2560x1600.png;
      indicator-x-position = 100;
      indicator-y-position = 100;
    };
  };
}
