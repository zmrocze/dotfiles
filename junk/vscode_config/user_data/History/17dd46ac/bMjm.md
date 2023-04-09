# how to generate cluster-data from scratch

Start inside the directory with files:
 - cluster-data/alonzo-genesis.yaml
 - cluster-data/byron-genesis.yaml
 - cluster-data/gen-byron-funds.sh
 - cluster-data/node.config
 - cluster-data/regenerate-byron.sh
 - cluster-data/regenerate.sh
 - cluster-data/shelley-genesis.yaml

 
 - Run "regenerate-byron.sh".
 - Modified byron-genesis.yaml.
 - Not remove delegate dir.
 - yq . alonzo-genesis.yaml > genesis.alonzo.spec.json.
 - Run regenerate.
 - Substitute genDelegs from regenerate.sh output.
 - Renamed byron deleg cert.
 - Run gen-byron-funds.
 - Substitute generated faucet addresses.

