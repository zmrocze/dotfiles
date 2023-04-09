#! /usr/bin/env bash


tmpDir=${TMPDIR:-/tmp}/build-17265750
declare -a args extraArgs



# Process the command line.
fetchOnly=
printFlags=
while [ $# -gt 0 ]; do
    arg="$1"
    shift
    if [ "$arg" = --help ]; then
        cat <<EOF
Usage: $0 [--dir PATH] [--run-env]

This script will reproduce Hydra build 17265750 of job Cardano:plutus-apps-pr-637:
(available at https://hydra.iohk.io/build/17265750).  It will fetch
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


inputDir="$tmpDir/plutus-apps/source"

args+=(--arg 'plutus-apps' "{ outPath = $inputDir; rev = \"bc1724c5e88197d3416fdf33190a52e916414993\"; shortRev = \"bc1724c\"; revCount = $revCount; }")


nixExprInputDir="$inputDir"

if [ -n "$inputDir" ]; then
    args+=(-I plutus-apps=$inputDir)
fi

inputDir=

args+=(--arg 'pr' '"637"') # FIXME: escape

if [ -n "$inputDir" ]; then
    args+=(-I pr=$inputDir)
fi


args+=(--option extra-binary-caches 'https://hydra.iohk.io/')

# Since Hydra runs on x86_64-linux, pretend we're one.  This matters
# when evaluating jobs that rely on builtins.currentSystem.
args+=(--option system x86_64-linux)

args+=("$nixExprInputDir/release.nix" -A '')

nix-build "${args[@]}" "${extraArgs[@]}"
