#!/usr/bin/env bash

set -euxo pipefail

# clear
# cd ../..
cabal run debug-run \
  "/tmp/test-cluster34157/bot-plutus-interface" \
  "/home/zmrocze/mlabs/bot-plutus-interface/binaries" \
  "/tmp/test-cluster34157/node/node.socket" \
  1097911063 \
  $1