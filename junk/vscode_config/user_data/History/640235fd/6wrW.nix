{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, packages ? import ./. { inherit system enableHaskellProfiling sources sourcesOverride; }
}:
let
  inherit (packages) pkgs;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt glibcLocales;
  inherit (plutus-apps) haskell stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cabal-fmt;
  
  nixpkgsInputs = with pkgs; [
    zlib
  ];
in 
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs;
}
