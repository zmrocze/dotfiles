#!/usr/env bash

line() {
	if [ "$2" = '_' ]; then 
		xargs -I $1 ${@[3,-1]}
	else 
		echo "expected '_'" && false
	fi
}

for i in $(seq 1 200); do
    cardano-cli \
        byron key signing-key-address \
        --byron-formats \
        --testnet-magic 764824073 \
        --secret faucet-addrs/faucet"$i".byron.key |
    head -n 1
done |
line xx _ echo - xx: '1000000000000000'
