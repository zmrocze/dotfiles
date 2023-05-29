{ pkgs, ... }: {
  imports = [ ./dconf.nix ];

  config = {

    home.packages = with pkgs; [ gnome.gnome-tweaks ];

    gtk = {
      enable = true;

      theme = {
        name = "arc";
        package = pkgs.arc-theme;
      };

      #     iconTheme = {
      # name = "Papirus-Dark";
      # package = pkgs.papirus-icon-theme;
      # };

      # cursorTheme = {
      #   name = "Numix-Cursor";
      #   package = pkgs.numix-cursor-theme;
      # };

      # gtk3.extraConfig = {
      #   Settings = ''
      #     gtk-application-prefer-dark-theme=1
      #   '';
      # };

      # gtk4.extraConfig = {
      #   Settings = ''
      #     gtk-application-prefer-dark-theme=1
      #   '';
      # };
    };

    # home.sessionVariables.GTK_THEME = "arc";

    # ...
    # dconf.settings = {
    #   # ...
    #   "org/gnome/shell" = {
    #     favorite-apps = [
    #       "firefox.desktop"
    #       "code.desktop"
    #       "org.gnome.Terminal.desktop"
    #       "spotify.desktop"
    #       "org.gnome.Nautilus.desktop"
    #     ];
    #     # `gnome-extensions list` for a list
    #     # enabled-extensions = [
    #     #   "user-theme@gnome-shell-extensions.gcampax.github.com"
    #     #   "trayIconsReloaded@selfmade.pl"
    #     #   "Vitals@CoreCoding.com"
    #     #   "dash-to-panel@jderose9.github.com"
    #     #   "sound-output-device-chooser@kgshank.net"
    #     #   "space-bar@luchrioh"
    #     # ];
    #   };
    #   "org/gnome/desktop/interface" = {
    #     color-scheme = "prefer-dark";
    #     # enable-hot-corners = false;
    #   };
    # };
  };
}
