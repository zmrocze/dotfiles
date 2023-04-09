#!/usr/bin/env bash

cd "$(dirname "$0")" || exit

bash update-proposal.sh "--protocol-major-version" "7" "--protocol-minor-version" "0"