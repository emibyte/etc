{pkgs, ...}: {
  services.kanshi = {
    enable = true;

    settings = let
      laptopScreenName = "eDP-1";
      disabledLaptopScreen = {
        criteria = laptopScreenName;
        status = "disable";
      };
      laptopScreen = {
        criteria = laptopScreenName;
        scale = 1.333333;
        mode = "2560x1600";
        position = "0,0";
        status = "enable";
      };

      home = let
        homeScreenName = "Ancor Communications Inc VG248 GALMQS113673";
        homeScreen = {
          criteria = homeScreenName;
          mode = "1920x1080";
          position = "0,0";
        };
      in [
        # home
        {
          profile = {
            name = "home";
            outputs = [
              disabledLaptopScreen
              homeScreen
            ];
          };
        }
      ];
    in
      [
        # laptop
        {
          profile = {
            name = "laptop";
            outputs = [laptopScreen];
          };
        }

        # inria
      ]
      ++ home;
  };
}
