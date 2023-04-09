# how to generate cluster-data from scratch

Start inside the directory with files:
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
 6. Substitute genDelegs from regenerate.sh output.
 7. Renamed byron deleg cert.
 8. Run gen-byron-funds.
 9. Substitute generated faucet addresses.

