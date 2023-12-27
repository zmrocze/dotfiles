#TODO just vscode extensions

regenerate-dconf-nix:
    nix run nixpkgs#dconf2nix -- -i home-manager/gnome/dconf_settings.helper -o home-manager/gnome/dconf.nix

# https://nixos.org/manual/nixos/stable/#sec-changing-config
build:
    just regenerate-dconf-nix
    nixos-rebuild build --flake .#omen
    nixos-rebuild build --flake .#omen-w-hm
    nix build .#homeConfigurations.zmrocze@omen.activationPackage

# full with hm
apply:
    just regenerate-dconf-nix
    bash apply.sh

# separate to home-manager
apply-system:
    bash apply-system.sh

# just hm
apply-home:
    just regenerate-dconf-nix
    bash apply-home.sh

pendlive-iso:
    nixos-generate --format iso --flake .#pendlive -o pendlive-iso
