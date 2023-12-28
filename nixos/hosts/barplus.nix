{ mypkgs, inputs, local-lib, lib, ... }: {
  imports = [
    ../general.nix
    ../barplus-hardware.nix
    ../../modules/username.nix
    (local-lib.hm-module-2-nixos-module {
      hm-module = import ../../home-manager/home.nix;
      extraSpecialArgs = {
        inherit inputs;
        username = "zmrocze";
      };
    })
  ];
  username = "zmrocze";
  hostname = "barplus";
  nixpkgs.pkgs = mypkgs;

  # overwrites to general
  boot.loader.timeout = lib.mkForce 10;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
