{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, packages ? import ./. { inherit system enableHaskellProfiling sources sourcesOverride; }
}:
let
  inherit (packages) pkgs plutus-apps plutus-playground pab-nami-demo docs webCommon;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt glibcLocales;
  inherit (plutus-apps) haskell stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cabal-fmt;
  nixpkgsInputs = with pkgs; [
    zlib
  ];
in 
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs;
  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;

  shellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  ''
  + ''
    export WEB_COMMON_SRC=${webCommon.cleanSrc}
  '';

  LOCALE_ARCHIVE = lib.optionalString
    (stdenv.hostPlatform.libc == "glibc")
    "${glibcLocales}/lib/locale/locale-archive";
}
