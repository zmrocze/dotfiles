{ inputs, pkgsFor, ... }:
let
  username = "test";
  hostname = "pendlive";
  system = "x86_64-linux";
in inputs.nixpkgs.lib.nixosSystem {
  specialArgs = { inherit inputs; }; # Pass flake inputs to our config
  # > Our main nixos configuration file <
  modules = [
    # ./nixos/configuration.nix
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares-gnome.nix"
    inputs.home-manager.nixosModules.home-manager
    ({ pkgs, ... }: {
      imports = [ ../../modules/usernames.nix ];
      home-manager = {
        useGlobalPkgs = true;
        extraSpecialArgs = { inherit inputs username; };
        users.${username} = { ... }: {
          imports = [ ../../home-manager/home.nix ];
        };
      };
      users.users = {
        "${username}" = {
          initialPassword = "password1";
          isNormalUser = true;
          extraGroups = [ "wheel" "audio" "networkmanager" ];
          shell = pkgs.zsh;
        };
      };
      programs.dconf.enable = true;
      programs.zsh.enable = true;
      inherit username hostname;
      nixpkgs.hostPlatform = system;
      nixpkgs.pkgs = pkgsFor system;
    })
  ];
}
