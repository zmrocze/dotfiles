#!/bin/sh

# to initilize without home-manager installed:
# nix run .#homeConfigurations.zmrocze@omen.activationPackage

pushd ~/dotfiles || exit
home-manager switch --flake '.#zmrocze@omen'
popd || exit
