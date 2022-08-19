{pkgs, ...}:
let my-theme = import ./theme.nix;
in {
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      pulseSupport = true;
      iwSupport = true;
    };
    script = "";
    config = {
      "bar/bar" = {
        background = my-theme.colors.bg;
        foreground = my-theme.colors.fg;
        font-0 = my-theme.fonts.normal-fontspec;
        font-1 = my-theme.fonts.large-fontspec;
        bottom = true;
        witdh = "100%";
        height = "23px";
        separator = "|";
        tray-padding = 2;
        tray-position = "right";
        tray-maxsize = 15;
        module-margin = 1;
        modules-left = "power home xmonad";
        modules-right = "network keyboard cpu mem date";
      };

      "module/home" = {
        type = "custom/text";
        content = "%{T2}"
                + "%{A:rofi -show p -modi p\\\\:rofi-power-menu:}襤%{A} "
                + "%{A:caja .:}%{A} "
                + "%{A:firefox:}爵%{A} "
                + "%{A:mate-control-center:}%{A}"
                + "%{T-}";
        format-content-font = 2;
        content-padding = 1;
      };

      "module/date" = {
        type = "internal/date";
        internal = 5;
        date = "%a, %b %d";
        time = "%H:%M";
        label = "%date% - %time%";
      };

      "module/mem" = {
        type = "internal/memory";
        interval = 3;
        label = "Mem: %{F${my-theme.colors.notify-ok}}%percentage_used%%{F-}%";
        label-warn = "Mem: %{F${my-theme.colors.notify-warn}}%percentage_used%%{F-}%";
        format-warn = "<label-warn>";
        warn-percentage = 80;
      };

      "module/cpu" = {
        type = "internal/cpu";
        interval = 3;
        label = "﬙ %{F${my-theme.colors.notify-ok}}%percentage%%{F-}%";
        label-warn = "﬙ %{F${my-theme.colors.notify-warn}}%percentage%%{F-}%";
        format-warn = "<label-warn>";
        warn-percentage = 85;
      };

      "module/keyboard" = {
        type = "internal/xkeyboard";
        blacklist-0 = "num lock";
        blacklist-1 = "scroll lock";
      };

      "module/volume" = {
        type = "internal/pulseaudio";
        format-volume = "<ramp-volume> <label-volume>";
        label-volume = "%percentage%%";
        label-volume-padding = "2px";
        ramp-volume-0 = "奄";
        ramp-volume-1 = "奔";
        ramp-volume-2 = "墳";
        label-muted = "奄 ﰸ ";
        click-right = "~/.nix-profile/bin/pavucontrol";
        use-ui-max = false;
        interval = 5;
      };

      "module/network" = {
        type = "internal/network";
        interface-type = "wireless";
        label-connected = "%ifname%: up %upspeed%; down %downspeed%";
        label-disconnected = "%{F${my-theme.colors.notify-warn}}wlan disconnected%{F-}";
      };

      "module/xmonad" = {
        type = "custom/script";
        exec = "tail -F /tmp/.xmonad-title-log";
        tail = true;
        interval = 0;
      };
    };
  };
}
