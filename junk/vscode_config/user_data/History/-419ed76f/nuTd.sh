#!/usr/bin/env bash

set -euxo pipefail

# clear
# cd ../..
cabal run debug-run \
  "/tmp/test-cluster45740/bot-plutus-interface" \
  "/home/zmrocze/mlabs/bot-plutus-interface/binaries" \
  "/tmp/test-cluster45740/node/node.socket" \
  764824073 \
  $1