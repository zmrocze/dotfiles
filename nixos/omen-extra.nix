# for omen
{ config, ... }: {
  services.syncthing = {
    enable = true;
    user = "${config.username}";
    dataDir =
      "/home/${config.username}/.Shared"; # Default folder for new synced folders
    configDir =
      "/home/${config.username}/.Shared/.config/syncthing"; # Folder for Syncthing's settings and keys
  };
}
