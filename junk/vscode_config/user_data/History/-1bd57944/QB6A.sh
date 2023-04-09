#!/bin/sh
VKEY_PATH=$1

MSG=$2

cd .. && echo $MSG | cabal run mk-plutus-v2-contracts && cd scripts

cardano-cli address build --payment-script-file ./ecdsaSecp256k1.plutus --mainnet > ./ecdsaSecp256k1.addr
cardano-cli address build --payment-script-file ./schnorrSecp256k1.plutus --mainnet > ./schnorrSecp256k1.addr
cardano-cli address build --payment-verification-key-file $VKEY_PATH --mainnet > ./ownWallet.addr

