#TODO just vscode extensions

regenerate-dconf-nix:
    nix run nixpkgs#dconf2nix -- -i home-manager/gnome/dconf_settings.helper -o home-manager/gnome/dconf.nix

build-home:
    nix build .#homeConfigurations.zmrocze@omen.activationPackage
