{pkgs, ...}: {
  services.kanshi = {
    enable = true;

    profiles = let
      mkSingleExternalScreen = {
        externalCriteria ? "DP-1",
        mode,
      }: {
        outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = externalCriteria;
            inherit mode;
            position = "0,0";
          }
        ];
      };
    in {
      laptop = {
        outputs = [
          {
            criteria = "eDP-1";
            mode = "2560x1600";
            position = "0,0";
            status = "enable";
          }
        ];
      };

      home = mkSingleExternalScreen {
        externalCriteria = "Ancor Communications Inc VG248 GALMQS113673";
        mode = "1920x1080";
      };
    };
  };
}
