{pkgs, ...}:
{
  programs.steam = {
    enable = true;
    # protontricks.enable = true;
    gamescopeSession.enable = true;
    remotePlay.openFirewall = true; # Open ports in firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
    extraPackages = with pkgs; [
      gamescope
      gamemode
    ];
    extraCompatPackages = with pkgs; [
      proton-ge-bin
    ];
  };

  programs.gamemode = {
    enable = true;
    settings.general.inhibit_screensaver = 0;
  };

  hardware.xone.enable = true;

  programs.gpu-screen-recorder.enable = true;
}
