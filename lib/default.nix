{ pkgs
#  ,inputs 
}: rec {
  concatFiles = pkgs.lib.strings.concatMap builtins.readFile;

  ifd = cmd: import (pkgs.runCommand "ifd-for-${cmd}" { } cmd);

  # Derviation producing directory like that:
  # mkDir {"offchain/src" : [some_derivation, other_derivation];
  #        "offchain": [./offchain]}
  # = 
  # /
  #   - offchain/
  #     - src/
  #       - some_derivation
  #       - other_derivation
  #     - ...
  # used instead of symlinkJoin, because https://github.com/nix-community/dream2nix/issues/520
  mkDir = name: dir:
    pkgs.runCommand name { } (''
      mkdir res
    '' + (pkgs.lib.concatMapStrings (path:
      let
        copyCmd = if path == "" then
          (drv: ''
            cp -r ${drv}/* res
          '')
        else
          (drv: ''
            mkdir -p res/${path}
            chmod +w res/${path}
            cp -r ${drv}/* res/${path}
          '');
      in pkgs.lib.concatMapStrings copyCmd dir.${path})
      (builtins.attrNames dir)) + ''
        mkdir $out
        cp -r res/* $out
      '');

  # Used to add pre-commit packages and shell hook to the other project shells
  mergeShells = devshell-1: devshell-2:
    pkgs.mkShell {
      packages = [ ];

      inputsFrom = [ devshell-1 devshell-2 ];

      shellHook = devshell-1.shellHook + devshell-2.shellHook;
    };

}
