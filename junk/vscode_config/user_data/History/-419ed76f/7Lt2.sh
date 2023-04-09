#!/usr/bin/env bash

set -euxo pipefail

# clear
cd ../..
cabal new-run exe:debug-run \
  "/home/mike/dev/mlabs/net-setups/testnet-bpi-setup/bpi" \
  "/home/mike/dev/mlabs/net-setups/testnet-bpi-setup/binaries" \
  "/tmp/test-cluster34157/node/node.socket" \
  1097911063 \
  $1