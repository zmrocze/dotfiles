#!/usr/env zsh
# Its zsh because of the list splice syntax,  

# Output from this command is to be added in byron-genesis.yaml

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
        --mainnet \
        --secret faucet-addrs/faucet"$i".byron.key |
    head -n 1
done |
line xx _ echo - xx: "'1000000000000000'"
