#!/usr/bin/env bash

# Inputs: $PAYMENT_KEY, $NUM_GENESIS_KEYS, $KEY_DIR, $TESTNET_MAGIC, $PROPOSAL_ARGS

# network=("--mainnet")
network=("--testnet-magic" "764824073")

# use some faucet address for fee
PAYMENT_KEY=faucet-addrs/faucet200
PAYMENT_SIGNING_KEY="$PAYMENT_KEY".shelley.key
# regenerate vkey
cardano-cli key verification-key --signing-key-file $PAYMENT_SIGNING_KEY --verification-key-file $PAYMENT_KEY.vkey

CHANGE_ADDRESS=$(cardano-cli address build "${network[@]}" --payment-verification-key-file "$PAYMENT_KEY".vkey )

TXIN=$(cardano-cli query utxo --address "$CHANGE_ADDRESS" "${network[@]}" --out-file /dev/stdout \
        | jq -r 'to_entries[0]|.key'
)
EPOCH=$(cardano-cli query tip  "${network[@]}" | jq .epoch)

PROPOSAL_ARGS=("--protocol-major-version" "7" "--protocol-minor-version" "0")

SIGNING_ARGS=()
PROPOSAL_KEY_ARGS=("--genesis-verification-key-file" "genesis-keys/genesis1.vkey")
# SIGNING_ARGS+=("--signing-key-file" "$KEY_DIR/delegate-keys/shelley.00$i.skey")


# cardano-cli transaction build \
#     --tx-in "$TXIN" \
#     --change-address "$CHANGE_ADDRESS" \
#     "${network[@]}" \
#     --out-file tx-proposal.txbody
cardano-cli transaction build \
    --tx-in "$TXIN" \
    --change-address "$CHANGE_ADDRESS" \
    --tx-out $(cardano-cli address build "${network[@]}" --verification-key-file faucet-addrs/faucet199.vkey)+3000000 \
    --witness-override 2 \
    "${network[@]}" \
    --out-file tx-proposal.txbody

cardano-cli transaction sign \
    --tx-body-file tx-proposal.txbody \
    --out-file tx-proposal.txsigned \
    --signing-key-file $PAYMENT_SIGNING_KEY --address "$CHANGE_ADDRESS" \

cardano-cli transaction submit \
    --tx-file tx-proposal.txsigned \
    "${network[@]}"