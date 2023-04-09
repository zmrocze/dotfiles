#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

bash update-proposal.sh "--cost-model-file" "cost-models-data/cost-model-v2.json"

# bash update-proposal.sh "--cost-model-file" $1