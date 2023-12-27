{ mypkgs, ... }: {
  imports = [
    ../general.nix
    ../hardware-configuration.nix
    ../omen-extra.nix
    ../../modules/username.nix
  ];
  username = "zmrocze";
  hostname = "omen";
  nixpkgs.pkgs = mypkgs;
}
