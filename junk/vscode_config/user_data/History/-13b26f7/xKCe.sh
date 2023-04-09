#! /usr/bin/env bash


args+=(--arg 'plutus-apps' "{ outPath = $inputDir; rev = \"bc1724c5e88197d3416fdf33190a52e916414993\"; shortRev = \"bc1724c\"; revCount = $revCount; }")


nixExprInputDir="$inputDir"



args+=(--arg 'pr' '"637"') # FIXME: escape

if [ -n "$inputDir" ]; then
    args+=(-I pr=$inputDir)
fi

args+=(--option extra-binary-caches 'https://hydra.iohk.io/')

# Since Hydra runs on x86_64-linux, pretend we're one.  This matters
# when evaluating jobs that rely on builtins.currentSystem.
args+=(--option system x86_64-linux)

args+=("$nixExprInputDir/release.nix" -A '')

echo nix-build "${args[@]}" "${extraArgs[@]}"
}

main "$@"
