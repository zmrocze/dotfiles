#TODO just vscode extensions

regenerate-dconf-nix:
    nix run nixpkgs#dconf2nix -- -i home-manager/gnome/dconf_settings.helper -o home-manager/gnome/dconf.nix

build-home:
    nix build .#homeConfigurations.zmrocze@omen.activationPackage

# https://nixos.org/manual/nixos/stable/#sec-changing-config
build:
    just regenerate-dconf-nix
    nixos-rebuild build --flake .#omen
    nix build .#homeConfigurations.zmrocze@omen.activationPackage

apply:
    just regenerate-dconf-nix
    bash apply-system.sh
    bash apply-home.sh

apply-home:
    just regenerate-dconf-nix
    bash apply-home.sh
