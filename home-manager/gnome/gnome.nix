{ pkgs, config, ... }: {
  imports = [ ./dconf.nix ];

  config = {

    home.packages = with pkgs; [ gnome-tweaks ];

    gtk = {
      enable = true;

      # theme = {
      #   name = "Arc-Darker";
      #   package = pkgs.arc-theme;
      # };

      iconTheme = {
        name = "Papirus-Dark-Maia";
        package = pkgs.papirus-maia-icon-theme;
      };

      cursorTheme = {
        name = "Bibata-Modern-Classic";
        package = pkgs.bibata-cursors;
      };

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
      "org/gnome/desktop/background" = rec {
        picture-options = "zoom";
        picture-uri = "file://${./wallpapers/manul.png}";
        picture-uri-dark = picture-uri;
      };
      # redundancy with dconf_settings.nix, but maybe needed
      "org/freedesktop/tracker/miner/files" = {
        index-recursive-directories = [ config.home.homeDirectory ];
      };
    };
  };
}
