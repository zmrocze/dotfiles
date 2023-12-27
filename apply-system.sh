#!/bin/sh
pushd ~/dotfiles || exit
sudo nixos-rebuild switch --flake .#omen
popd || exit

