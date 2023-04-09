# how to generate cluster-data from scratch

Start inside the cluster-data directory with files:
 - cluster-data/alonzo-genesis.yaml
 - cluster-data/shelley-genesis.yaml
 - cluster-data/byron-genesis.yaml
 - cluster-data/node.config
 - cluster-data/gen-byron-funds.sh
 - cluster-data/regenerate-byron.sh
 - cluster-data/regenerate.sh

Do:
 0. Create `genesis.alonzo.spec.json` with `yq . alonzo-genesis.yaml > genesis.alonzo.spec.json`.
 1. Run `regenerate-byron.sh`.
 2. It created `byron-genesis-init.yaml`, update `bootStakeholder` and `heavyDelegation` fields in byron-genesis.yaml to match it.
 5. Run `regenerate.sh`.
 6. Substitute output from previous command into `genDelegs` field in `shelley-genesis.yaml`.
 8. Run `mkdir faucet-addrs && bash gen-byron-funds.sh`.
 9. Substitute generated faucet addresses.

