{ pkgs, config, lib, ... }: {
  # TODO: understand this file lmao
  #       did this because for some reason as soon as im importing some
  #       some keybinds from this file all default keybinds from sway are gone.

  wayland.windowManager.sway = {
    config =
      let
        mod = "Mod4";
      in
      {
        keybindings = {
          # TODO: use swaylock-effects here with --clock
          "${mod}+F12" =
            let
              lock = pkgs.writeShellScript "lock" ''
                ${pkgs.swaylock}/bin/swaylock
              '';
            in
            "exec ${lock}";

          XF86MonBrightnessUp = "exec ${pkgs.light}/bin/light -A 10";
          XF86MonBrightnessDown = "exec ${pkgs.light}/bin/light -U 10";

          "${mod}+g" = "exec ${pkgs.pavucontrol}/bin/pavucontrol";
        }
        // (
          let
            pactl = "exec ${pkgs.pulseaudio}/bin/pactl";
            unmute = "${pactl} set-sink-mute @DEFAULT_SINK@ off";
            volumePrefix = "${pactl} set-sink-volume @DEFAULT_SINK@";
            commandPrefix = "${unmute}; ${volumePrefix}";

            volumeDown = "${commandPrefix} -5%";
            volumeUp = "${commandPrefix} +5%";
          in
          {
            "${mod}+Up" = volumeUp;
            XF86AudioRaiseVolume = volumeUp;

            "${mod}+Down" = volumeDown;
            XF86AudioLowerVolume = volumeDown;

            XF86AudioMute = "${pactl} set-sink-mute @DEFAULT_SINK@ toggle";
          }
        )
        // (
          let
            playerctl = "exec ${pkgs.playerctl}/bin/playerctl";

            next = "${playerctl} next";
            previous = "${playerctl} previous";
          in
          {
            XF86AudioPlay = "${playerctl} play-pause";
            XF86AudioPause = "${playerctl} pause";
            XF86AudioNext = next;
            "${mod}+Right" = next;
            XF86AudioPrev = previous;
            "${mod}+Left" = previous;
          }
        )
        // {
          # Toggle touchpad
          F9 =
            let
              showTouchpadState = pkgs.writeShellScript "show-touchpad-state" ''
                status=$(swaymsg -t get_inputs | ${pkgs.jq}/bin/jq --raw-output '.[] | select(.type=="touchpad") | .libinput.send_events')
                notify-send "Touchpad $status"
              '';
            in
            "input type:touchpad events toggle enabled disabled; exec ${showTouchpadState}";

          # Bluetooth
          "${mod}+i" = "exec ${pkgs.blueman}/bin/blueman-manager";
          "${mod}+Shift+i" = "exec doas rfkill unblock bluetooth";
          "${mod}+Ctrl+i" = "exec doas rfkill block bluetooth";

          # TODO: VPN
          # Wireguard (VPN)
          "${mod}+u" = "exec doas systemctl start wg-quick-wg0";
          "${mod}+Shift+u" = "exec doas systemctl stop wg-quick-wg0";

          # TODO: Dunst
          # Dunst (notifications)
          # "${mod}+n" = "exec ${pkgs.mako}/bin/makoctl set-mode silent";
          # "${mod}+Shift+n" = "exec ${pkgs.mako}/bin/makoctl set-mode default";
          "${mod}+n" = "exec ${pkgs.dunst}/bin/dunstctl set-paused false";
          "${mod}+Shift+n" = "exec ${pkgs.dunst}/bin/dunstctl set-paused true";
        }
        ################
        # Applications #
        ################
        // {
          # Terminal
          "${mod}+Return" = "exec ${pkgs.wezterm}/bin/wezterm";

          # Rofi
          "${mod}+d" = "exec ${config.programs.rofi.package}/bin/rofi -show run";

          # Web browser
          "${mod}+w" = "exec ${pkgs.firefox}/bin/firefox";

          # screenshot (flameshot)
          # "Print" = "exec ${pkgs.flameshot}/bin/flameshot gui";
          # TODO switch back to normal mapping (above) when issue is fixed
          "Print" = "exec ${pkgs.flameshot}/bin/flameshot gui --raw | wl-copy";

          # file manager
          "${mod}+F3" = "exec ${pkgs.xfce.thunar}/bin/thunar";
        }
        ########
        # Sway #
        ########
        // {
          # quit/restart sway
          "${mod}+Shift+a" = "exec ${pkgs.sway}/bin/swaymsg exit";
          "${mod}+Shift+r" = "reload";

          # kill window
          "${mod}+Shift+q" = "kill";

          # alternate between the tiled and monocle layout
          "${mod}+z" = "layout toggle splith tabbed";

          # Cycle through the nodes (leaves only) of the current desktop
          "${mod}+space" = "exec ${pkgs.i3-cycle-focus}/bin/i3-cycle-focus cycle";

          # Back and forth desktop
          "${mod}+b" = "workspace back_and_forth";

          "${mod}+Shift+p" = "move workspace to output right";

          # STATE/FLAGS

          "${mod}+f" = "fullscreen toggle";

          "${mod}+Shift+space" = "floating toggle";

          "${mod}+v" = "layout toggle splitv splith";

          # FOCUS/SWAP

          # focus the node in the given direction
          "${mod}+h" = "focus left";
          "${mod}+j" = "focus down";
          "${mod}+k" = "focus up";
          "${mod}+l" = "focus right";
          "${mod}+Shift+h" = "move left";
          "${mod}+Shift+j" = "move down";
          "${mod}+Shift+k" = "move up";
          "${mod}+Shift+l" = "move right";

          # focus the next/previous workspace
          "${mod}+ctrl+h" = "workspace prev";
          "${mod}+ctrl+l" = "workspace next";
          "ctrl+Alt+h" = "workspace prev";
          "ctrl+Alt+l" = "workspace next";

          # resize a window
          "${mod}+r" = "mode resize";

          "${mod}+a" = "workspace number 1";
        }
        # TODO: im still not sure how this works but it does
        // (
          lib.listToAttrs (
            lib.flatten (
              lib.mapAttrsToList
                (
                  wsNumber: key: [
                    {
                      name = "${mod}+${key}";
                      value = "workspace number ${wsNumber}";
                    }
                    {
                      name = "${mod}+Shift+${key}";
                      value = "move container to workspace number ${wsNumber}";
                    }
                  ]
                )
                {
                  "1" = "1";
                  "2" = "2";
                  "3" = "3";
                  "4" = "4";
                  "5" = "5";
                  "6" = "6";
                  "7" = "7";
                  "8" = "8";
                  "9" = "9";
                  "10" = "0";
                }
            )
          )
        );

        modes = {
          resize = {
            h = "resize shrink width 20 px";
            j = "resize grow height 20 px";
            k = "resize shrink height 20 px";
            l = "resize grow width 20 px";
            Escape = "mode default";
            Return = "mode default";
          };
        };
      };
  };
}