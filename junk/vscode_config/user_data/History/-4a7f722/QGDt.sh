#!/usr/bin/env bash

cd "$(dirname "$0")" || exit
pwd
# wait first for babbage era
while ! cardano-cli query tip --mainnet | grep 'Babbage' ; do sleep 0.1 ; done

bash update-proposal.sh "--babbage-era" "--cost-model-file" "cost-models-data/cost-model-v2.json"

# bash update-proposal.sh "--babbage-era" "--cost-model-file" $1