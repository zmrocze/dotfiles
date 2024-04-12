{ mypkgs, ... }: {
  imports = [
    ../general.nix
    ../hardware/framework.nix
    ../omen-extra.nix
    ../../modules/username.nix
    ../virtualization.nix
  ];
  username = "zmrocze";
  hostname = "omen";
  nixpkgs.pkgs = mypkgs;
}
