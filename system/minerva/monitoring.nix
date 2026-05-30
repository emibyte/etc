{...}: {
  programs.tuxclocker = {
    # TODO(emi): 26.05 build failure dont care dont ask
    enable = false;
    useUnfree = true;
  };

  programs.coolercontrol = {
    enable = true;
  };
}
