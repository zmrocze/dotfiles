#!/usr/bin/env bash

set -euxo pipefail


# export CARDANO_NODE_SOCKET_PATH=/home/mike/dev/mlabs/net-setups/testnet-bpi-setup/socket/forwarded-node.socket
export CARDANO_NODE_SOCKET_PATH="/tmp/test-cluster45740/node/node.socket"
export PATH="/home/zmrocze/mlabs/bot-plutus-interface/binaries:$PATH"
export MAGIC=764824073
cardano-cli query tip --testnet-magic $MAGIC
