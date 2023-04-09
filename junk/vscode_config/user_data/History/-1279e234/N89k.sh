#!/bin/sh
set -e

ALGO=$1
SKEY_PATH=$2

ADDR=$(cat ./ownWallet.addr)
SCRIPT_ADDR=$(cat ./${ALGO}Secp256k1.addr)

cardano-cli query protocol-parameters --out-file protocol.json --mainnet

cardano-cli query utxo --address $ADDR --mainnet --out-file ownUtxos.json
TX_IN=$(cat ownUtxos.json | jq -r "keys | .[0]")

cardano-cli query utxo --address $SCRIPT_ADDR --mainnet --out-file scriptUtxos.json
SCRIPT_TX_IN=$(cat scriptUtxos.json | jq -r "keys | .[0]")

cardano-cli transaction build \
  --babbage-era \
  --tx-in-collateral $TX_IN \
  --tx-in $TX_IN \
  --tx-in $SCRIPT_TX_IN \
  --tx-in-script-file ./${ALGO}Secp256k1.plutus \
  --tx-in-datum-file ./unitDatum.json \
  --tx-in-redeemer-file ./${ALGO}Secp256k1Redeemer.json \
  --change-address $ADDR \
  --mainnet \
  --protocol-params-file protocol.json \
  --out-file tx.raw 

if [ ! -z $SKEY_PATH ]; then
  cardano-cli transaction sign --signing-key-file $SKEY_PATH --tx-body-file tx.raw --out-file tx.signed

  cardano-cli transaction submit --tx-file tx.signed --mainnet
fi
