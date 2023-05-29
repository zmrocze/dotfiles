{ pkgs, ... }: {
  imports = [ ./dconf.nix ];

  config = {

    home.packages = with pkgs; [ gnome.gnome-tweaks ];

    gtk = {
      enable = true;

      # theme = {
      #   name = "Arc-Darker";
      #   package = pkgs.arc-theme;
      # };

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

    # home.sessionVariables.GTK_THEME = "Arc-Darker";

    # extra dconf settings
    dconf.settings = {
      "org/gnome/desktop/background" = {
        picture-options = "zoom";
        picture-uri = "file://${./wallpapers/manul.png}";
      };
    };
  };
}
