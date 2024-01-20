# How to create new PC?

## base

1. Set up partitions (and swap? i think not needed <- not needed ).
2. Generate hardware.nix - this part of the configuration is only somewhat portable.
3. Merge hardware.nixs and apply this configuration

## extras

4. Login into vscode with your github account
5. Login into firefox
6. Use the toolchains (rustup/ghcup/coursier) to install the compilers you need. ghcup & coursier are not installed
7. cachix use mlabs / copy netrc

## data

Well, copy/sync the data you need. You might find archive/some_archived_configs_manjaro/list_of_dirs somewhat helpful.

## Refind 

Just run refind-install to get basic refind.
Then I've changed the theme: copy it into somewhere `Boot/efi/refind/themes/minimal` and include in `refind.conf`.
Then I've changed the icon for systemd boot to the icon for nixos boot by sth like overwriting `cp icons/os_nixos.png icons/os_systemd.png`.

## luks encrypt (barplus)

Thats software layer encryption, that is using device mapper.

## using windows VSTs

Use `yabridgectl` (should be installed). Process:

1. Install the vst under wine. Can use just the windows installer running with wine. But basically make it showup somewhere in `~/.wine/...../my.vst3`.
2. run `yabridgectl add <path to vst's directory>`
3. run `yabridgectl sync`
4. The linux compatible vst should showup in `~/.vst3/yabridge` or sth
5. AAAAAnd doesnt show up in daw...