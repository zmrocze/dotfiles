{ inputs, mypkgs, mylib, config, lib, ... }:
let
  username = "test";
  hostname = "pendlive";
in {
  imports = [
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
    # Provide an initial copy of the NixOS channel so that the user
    # doesn't need to run "nix-channel --update" first.
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"

    ../general.nix

    (mylib.hm-module-2-nixos-module {
      hm-module = import ../../home-manager/hosts/default.nix;
      extraSpecialArgs = { inherit username inputs; };
    })

    ../../modules/username.nix
  ];

  inherit username hostname;
  users.users = {
    "${config.username}" = {
      # If you set it, you can skip setting a root password by passing '--no-root-passwd' to nixos-install.
      initialPassword = lib.mkForce "";
    };
  };

  nixpkgs.pkgs = mypkgs;

  # overwrites to general
  boot.loader.timeout = lib.mkForce 10;

  # installation-cd-minimal induced overwrites
  services.openssh.enable = lib.mkForce false;
  networking.wireless.enable = lib.mkForce false;

}
