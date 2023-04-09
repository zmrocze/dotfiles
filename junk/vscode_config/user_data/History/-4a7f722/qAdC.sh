#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

bash update-proposal.sh "--babbage-era" "--cost-model-file" "cost-models-data/cost-model-v2.json"

# bash update-proposal.sh "--babbage-era" "--cost-model-file" $1