{ mypkgs, ... }: {
  imports = [
    ../general.nix
    ../omen-hardware.nix
    ../omen-extra.nix
    ../../modules/username.nix
    ../virtualization.nix
  ];
  username = "zmrocze";
  hostname = "omen";
  nixpkgs.pkgs = mypkgs;
}
