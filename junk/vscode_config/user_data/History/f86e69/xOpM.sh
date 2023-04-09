#! /usr/bin/env bash

main() {

# This script has been generated automatically by Hydra from the build
# at https://hydra.iohk.io/build/17573192.

set -e

tmpDir=${TMPDIR:-/tmp}/build-17573192
declare -a args extraArgs


info() {
    echo "[1;32m$1[0m" >&2
}


# Process the command line.
fetchOnly=
printFlags=
while [ $# -gt 0 ]; do
    arg="$1"
    shift
    if [ "$arg" = --help ]; then
        cat <<EOF
Usage: $0 [--dir PATH] [--run-env]

This script will reproduce Hydra build 17573192 of job Cardano:plutus-pr-4597:
(available at https://hydra.iohk.io/build/17573192).  It will fetch
all inputs of the Hydra build, then invoke Nix to build the job and
all its dependencies.

The inputs will be stored in $tmpDir.  This can be overriden using the
--dir flag.  After the build, the result of the build is available via
the symlink $tmpDir/result.

Flags:

  --dir PATH
    Override the location where the inputs and result symlink are stored.

  --run-env
    Fetch the inputs and build the dependencies, then start an
    interactive shell in which the environment is equal to that used
    to perform the build.  See the description of the --run-env flag
    in the nix-build(1) manpage for more details.

  --fetch
    Fetch the inputs and then exit.

  --print-flags
    Fetch the inputs, then print the argument to nix-build on stdout
    and exit.

Any additional flags are passed to nix-build.  See the nix-build(1)
manpage for details.
EOF
        exit 0
    elif [ "$arg" = --dir ]; then
        tmpDir="$1"
        if [ -z "$tmpDir" ]; then
            echo "$0: --dir requires an argument" >&2
            exit 1
        fi
        shift
    elif [ "$arg" = --fetch ]; then
        fetchOnly=1
    elif [ "$arg" = --print-flags ]; then
        printFlags=1
    else
        extraArgs+=("$arg")
    fi
done


mkdir -p "$tmpDir"
cd "$tmpDir"
info "storing inputs and results in $tmpDir..."


requireCommand() {
    local cmd="$1"
    if ! type -P "$cmd" > /dev/null; then
        echo "$0: command ‘$cmd’ is not installed; please install it and try again" >&2
        exit 1
    fi
    return 0
}


# Fetch the inputs.

inputDir=


inputDir="$tmpDir/plutus/source"

if ! [ -d "$inputDir" ]; then
    info "fetching Git input ‘plutus’ from ‘https://github.com/zmrocze/plutus.git’ (commit 3662d294d89316d46d8f2d711d19949199714d21)..."
    requireCommand git
    inputDirTmp="$inputDir.tmp"
    rm -rf "$inputDirTmp"
    mkdir -p "$inputDirTmp"
    git clone 'https://github.com/zmrocze/plutus.git' "$inputDirTmp"
    (cd "$inputDirTmp" && git checkout '3662d294d89316d46d8f2d711d19949199714d21')
    revCount="$(cd "$inputDirTmp" && (git rev-list '3662d294d89316d46d8f2d711d19949199714d21' | wc -l))"
    rm -rf "$inputDirTmp/.git"
    mv "$inputDirTmp" "$inputDir"
    echo -n $revCount > "$tmpDir/plutus/rev-count"
else
    revCount="$(cat "$tmpDir/plutus/rev-count")"
fi

args+=(--arg 'plutus' "{ outPath = $inputDir; rev = \"3662d294d89316d46d8f2d711d19949199714d21\"; shortRev = \"3662d29\"; revCount = $revCount; }")


nixExprInputDir="$inputDir"

if [ -n "$inputDir" ]; then
    args+=(-I plutus=$inputDir)
fi

inputDir=

args+=(--arg 'pr' '"4597"') # FIXME: escape

if [ -n "$inputDir" ]; then
    args+=(-I pr=$inputDir)
fi


if [ -n "$fetchOnly" ]; then exit 0; fi


# Run nix-build.

requireCommand nix-build

if [ -z "$nixExprInputDir" ]; then
    echo "$0: don't know the path to the Nix expression!" >&2
    exit 1
fi

args+=(--option extra-binary-caches 'https://hydra.iohk.io/')

# Since Hydra runs on x86_64-linux, pretend we're one.  This matters
# when evaluating jobs that rely on builtins.currentSystem.
args+=(--option system x86_64-linux)

args+=("$nixExprInputDir/release.nix" -A '')

if [ -n "$printFlags" ]; then
    first=1
    for i in "${args[@]}"; do
        if [ -z "$first" ]; then printf " "; fi
        first=
        printf "%q" "$i"
    done
    exit 0
fi

info "running nix-build..."
echo "using the following invocation:" >&2
set -x
nix-build "${args[@]}" "${extraArgs[@]}"
}

main "$@"
