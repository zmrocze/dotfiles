# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/desktop/peripherals/touchpad" = {
      click-method = "areas";
    };

    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };

    "org/gnome/desktop/peripherals/mouse" = {
      speed = 0.2365145228215768;
    };

    "org/gnome/desktop/wm/keybindings" = {
      close = [ "<Alt>F4" ];
      maximize = [ "<Super>Up" ];
      switch-applications = [];
      switch-applications-backward = [];
      switch-to-workspace-1 = [ "<Super>1" ];
      switch-to-workspace-10 = [ "<Super>0" ];
      switch-to-workspace-2 = [ "<Super>2" ];
      switch-to-workspace-3 = [ "<Super>3" ];
      switch-to-workspace-4 = [ "<Super>4" ];
      switch-to-workspace-5 = [ "<Super>5" ];
      switch-to-workspace-6 = [ "<Super>6" ];
      switch-to-workspace-7 = [ "<Super>7" ];
      switch-to-workspace-8 = [ "<Super>8" ];
      switch-to-workspace-9 = [ "<Super>9" ];
      switch-to-workspace-left = [ "<Super><Shift>Tab" ];
      switch-to-workspace-right = [ "<Super>Tab" ];
      switch-windows = [ "<Alt>Tab" ];
      switch-windows-backward = [ "<Shift><Alt>Tab" ];
    };

    "org/gnome/shell/keybindings" = {
      switch-to-application-1 = [];
      switch-to-application-2 = [];
      switch-to-application-3 = [];
      switch-to-application-4 = [];
      switch-to-application-5 = [];
      switch-to-application-6 = [];
      switch-to-application-7 = [];
      switch-to-application-8 = [];
      switch-to-application-9 = [];
    };

    "org/gnome/shell" = {
      favorite-apps = [ "firefox.desktop" "gnome-terminal.desktop" "org.gnome.Nautilus.desktop" "spotify.desktop" ];
    };

    "org/freedesktop/tracker/miner/files" = {
      index-recursive-directories = [];
      index-single-directories = [ "$HOME" "/home/zmrocze" "&DOWNLOAD" ];
    };

    "org/gnome/terminal/legacy/profiles:" = {
      default = "33dd0588-9ee4-4da4-969f-133ce73633df";
      list = [ "33dd0588-9ee4-4da4-969f-133ce73633df" ];
    };

    "org/gnome/terminal/legacy/profiles:/:33dd0588-9ee4-4da4-969f-133ce73633df" = {
      font = "Monospace 24";
      preserve-working-directory = "never";
      use-system-font = false;
      use-theme-colors = true;
      visible-name = "alright_size";
    };

    "org/gnome/desktop/session" = {
      idle-delay = 720;
    };

    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enable = true;
    };

  };
}
