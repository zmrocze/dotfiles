# How to create new PC?

## base

1. Set up partitions (and swap? i think not needed).
2. Generate hardware.nix - this part of the configuration is only somewhat portable.
3. Merge hardware.nixs and apply this configuration

## extras

4. Login into vscode with your github account
5. Login into firefox
6. Use the toolchains (rustup/ghcup/coursier) to install the compilers you need. ghcup & coursier are not installed

## data

Well, copy/sync the data you need. You might find archive/some_archived_configs_manjaro/list_of_dirs somewhat helpful.