#!/usr/env bash
for i in $(seq 1 200); do
cardano-cli \
    byron key signing-key-address \
    --byron-formats \
    --testnet-magic 764824073 \
    --secret faucet-addrs/faucet"$i".byron.key |
head -n 1
done |
line xx _ echo - xx: '1000000000000000'
