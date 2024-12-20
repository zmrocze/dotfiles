{
  description = "Your new nix config";

  # nixConfig = { allow-import-from-derivation = true; };

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-23-05.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-24-05.url = "github:nixos/nixpkgs/nixos-24.05";
    nur.url = "github:nix-community/NUR";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # TODO: Add any other flake you might need
    # hardware.url = "github:nixos/nixos-hardware";

    # Shameless plug: looking for a way to nixify your themes and make
    # everything match nicely? Try nix-colors!
    # nix-colors.url = "github:misterio77/nix-colors";

    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    my-lib.url = "github:zmrocze/nix-lib";

    powerlevel10k = {
      url = "github:romkatv/powerlevel10k";
      flake = false;
    };

    zsh-directory-history = {
      url = "github:tymm/zsh-directory-history?ref=master";
      flake = false;
    };

    musnix.url = "github:musnix/musnix";
    # zsh-autocomplete = {
    #   url = "github:marlonrichert/zsh-autocomplete";
    #   flake = false;
    # };
  };

  outputs = { nixpkgs, nixpkgs-23-05, home-manager, flake-parts, my-lib
    , nixpkgs-24-05, nixpkgs-unstable, ... }@inputs:
    let
      local-lib' = import ./lib { inherit inputs; };
      local-lib = local-lib'.pure // my-lib.lib;
      inherit (import ./pkgs {
        inherit nixpkgs nixpkgs-23-05 nixpkgs-24-05 nixpkgs-unstable my-lib;
      })
        pkgsFor;
      hostname = "omen";
      username = "zmrocze";
      system = "x86_64-linux";
    in flake-parts.lib.mkFlake { inherit inputs; } rec {
      imports = [ ./pre-commit.nix ];
      systems = [ system ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = pkgsFor system;
        devShells = {
          default = pkgs.mergeShells config.devShells.pre-commit (pkgs.mkShell {
            packages = [ pkgs.just ];
            # inputsFrom = [ pkgs.hello ];
            # shellHook = '''';
          });
        };
        checks = {
          "homeConfiguraton_zmrocze@omen" =
            flake.homeConfigurations."zmrocze@omen".activationPackage;
        };
      };
      flake = {

        # NixOS configuration entrypoint
        # Available through 'nixos-rebuild --flake .#your-hostname'
        nixosConfigurations = {
          "omen" = nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs;
              mypkgs = pkgsFor system;
            };
            modules = [ ./nixos/hosts/omen.nix ];
          };
          "pendlive" = nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs;
              mypkgs = pkgsFor system;
              mylib = local-lib;
            };
            modules = [ ./nixos/hosts/pendlive.nix ];
          };
          "omen-w-hm" = nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs;
              mypkgs = pkgsFor system;
            };
            modules = [
              ./nixos/hosts/omen.nix
              (local-lib.hm-module-2-nixos-module {
                hm-module = import ./home-manager/home.nix;
                extraSpecialArgs = { inherit inputs username; };
              })
            ];
          };
          "barplus" = nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs local-lib;
              mypkgs = pkgsFor system;
            };
            modules = [ ./nixos/hosts/barplus.nix ];
          };
        };

        # Standalone home-manager configuration entrypoint
        # Available through 'home-manager --flake .#your-username@your-hostname'
        homeConfigurations = {
          "${username}@${hostname}" =
            home-manager.lib.homeManagerConfiguration {
              pkgs = pkgsFor system;
              extraSpecialArgs = { inherit username inputs; };
              modules = [ (import ./home-manager/home.nix) ];
            };
        };
      };
    };
}

