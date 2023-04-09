#!/usr/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd "$DIR/.." || exit

touch miner-logs1
touch miner-logs2
touch wallet-logs

stack build

gnome-terminal --tab -- sh -c 'stack run -- miner --config app/configs/miner_config1.yaml & tail -f miner-logs1'
gnome-terminal --tab -- sh -c 'stack run -- miner --config app/configs/miner_config2.yaml & tail -f miner-logs2'

gnome-terminal --tab -- sh -c 'stack run -- wallet --config app/configs/wallet_config.yaml & tail -f wallet-logs'
