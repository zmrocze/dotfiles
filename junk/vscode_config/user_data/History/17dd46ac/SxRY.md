# how to generate cluster-data from scratch

 - Clear cluster-data. Start.
 - Run "regenerate-byron.sh".
 - Modified byron-genesis.yaml.
 - Not remove delegate dir.
 - yq . alonzo-genesis.yaml > genesis.alonzo.spec.json.
 - Run regenerate.
 - Substitute genDelegs from regenerate.sh output.
 - Renamed byron deleg cert.
 - Run gen-byron-funds.
 - Substitute generated faucet addresses.

