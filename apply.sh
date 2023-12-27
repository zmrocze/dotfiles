#!/bin/sh
pushd ~/dotfiles || exit
sudo nixos-rebuild switch --flake .#omen-w-hm
popd || exit