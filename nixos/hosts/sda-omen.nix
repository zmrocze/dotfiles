# KAROL:
# so im using the old disk as backup
# but actually decided to install something bootable there, why not

# This is like omen-hardware.nix but different filesystems

# Password set to "password"

{ mypkgs, ... }: {
  imports = [
    ../general.nix
    ../hardware/sda-omen.nix
    ../omen-extra.nix
    ../../modules/username.nix
    ../virtualization.nix
  ];
  username = "zmrocze";
  hostname = "omen";
  nixpkgs.pkgs = mypkgs;
}
