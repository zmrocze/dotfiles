{
  description = "Your new nix config";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    nur.url = "github:nix-community/NUR";

    # Home manager
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # TODO: Add any other flake you might need
    # hardware.url = "github:nixos/nixos-hardware";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";

    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    powerlevel10k = {
      url = "github:romkatv/powerlevel10k";
      flake = false;
    };

  };

  outputs = { nixpkgs, home-manager, flake-parts, pre-commit-hooks
    , powerlevel10k, ... }@inputs:
    let
      hostname = "omen";
      username = "zmrocze";
      system = "x86_64-linux";
      pre-commit-module = { inputs, lib, ... }: {
        imports = [
          pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
        ];
        perSystem = { pkgs, system, inputs', config, ... }: {
          devShells.default = config.pre-commit.devShell;
          pre-commit.settings = rec {
            excludes = [ 
              "./junk.*"
              "junk.*"
            ];
            hooks = {
              nixfmt.enable = true;
              statix = {
                enable = true;
                ignore = excludes;
              };
              deadnix.enable = true;
              shellcheck.enable = true;
            };
          };
        };
      };
    in flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ pre-commit-module ];
      systems = [ system ];
      flake = {

        # NixOS configuration entrypoint
        # Available through 'nixos-rebuild --flake .#your-hostname'
        nixosConfigurations = {
          # FIXME replace with your hostname
          "${hostname}" = nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs username hostname;
            }; # Pass flake inputs to our config
            # > Our main nixos configuration file <
            modules = [ ./nixos/configuration.nix ];
          };
        };

        # Standalone home-manager configuration entrypoint
        # Available through 'home-manager --flake .#your-username@your-hostname'
        homeConfigurations = {
          # FIXME replace with your username@hostname
          "${username}@${hostname}" =
            home-manager.lib.homeManagerConfiguration {
              pkgs =
                nixpkgs.legacyPackages.${system}; # Home-manager requires 'pkgs' instance
              extraSpecialArgs = {
                inherit inputs username;
              }; # Pass flake inputs to our config
              # > Our main home-manager configuration file <
              modules = [ ./home-manager/home.nix ];
            };
        };
      };
    };
}
