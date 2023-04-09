{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, packages ? import ./. { inherit system enableHaskellProfiling sources sourcesOverride; }
}:
let
  inherit (packages) pkgs plutus-apps plutus-playground pab-nami-demo docs webCommon;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt glibcLocales;
  inherit (plutus-apps) haskell;
  nixpkgsInputs = with pkgs; [
    zlib
  ];
in 
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs;
}
