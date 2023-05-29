# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      xkb-options = [ "ctrl:nocaps" ];
    };

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
      switch-windows = [ "<Alt>Tab" ];
      switch-windows-backward = [ "<Shift><Alt>Tab" ];
    };

  };
}
