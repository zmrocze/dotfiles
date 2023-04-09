#!/usr/bin/env bash

set -euxo pipefail

# clear
# cd ../..
# pwd
cabal new-run exe:debug-run \
  "/tmp/test-cluster45740/bot-plutus-interface" \
  "/home/zmrocze/mlabs/bot-plutus-interface/binaries" \
  "/tmp/test-cluster45740/node/node.socket" \
  0 \
  $1