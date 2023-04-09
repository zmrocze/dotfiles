{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, packages ? import ./. { inherit system enableHaskellProfiling sources sourcesOverride; }
}:
let
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt glibcLocales;

  nixpkgsInputs = with pkgs; [
    zlib
  ];
in 
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs;
}
