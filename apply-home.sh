#!/bin/sh

# to initilize without home-manager installed:
# nix run .#homeConfigurations.zmrocze@omen.activationPackage

pushd ~/dotfiles
home-manager switch --flake '.#zmrocze@omen'
popd
