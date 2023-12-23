{
  description = "Your new nix config";

  # nixConfig = { allow-import-from-derivation = true; };

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-23-05.url = "github:nixos/nixpkgs/nixos-23.05";
    nur.url = "github:nix-community/NUR";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/release-23.11";
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
    # zsh-autocomplete = {
    #   url = "github:marlonrichert/zsh-autocomplete";
    #   flake = false;
    # };
  };

  outputs = { nixpkgs, home-manager, flake-parts, pre-commit-hooks, my-lib, ...
    }@inputs:
    let
      hostname = "omen";
      username = "zmrocze";
      system = "x86_64-linux";
      pkgsFor = system:
        import inputs.nixpkgs {
          inherit system;
          overlays = [
            (final: _: my-lib.lib final)
            (_: _:
              let pkgs2305 = pkgs2305For system;
              in {
                inherit (pkgs2305) aliza;
                nixpkgs-23-05.haskellPackages.cabal-plan =
                  pkgs2305.haskellPackages.cabal-plan;
              })
          ];
          config = {
            allowUnfree = true;
            allowUnfreePredicate = _: true;
          };
        };
      pkgs2305For = system:
        import inputs.nixpkgs-23-05 {
          inherit system;
          config = {
            allowUnfree = true;
            allowUnfreePredicate = _: true;
          };
        };
      pre-commit-module = {
        imports = [
          pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
        ];
        perSystem = { config, ... }: {
          devShells.pre-commit = config.pre-commit.devShell;
          pre-commit.settings = rec {
            excludes = [ "junk" "home-manager/gnome/dconf.nix" ];
            hooks = {
              nixfmt.enable = true;
              statix = { enable = true; };
              deadnix.enable = true;
              shellcheck.enable = true;
            };
            settings.statix.ignore = excludes;
          };
        };
      };
    in flake-parts.lib.mkFlake { inherit inputs; } rec {
      imports = [ pre-commit-module ];
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
          "${hostname}" = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; }; # Pass flake inputs to our config
            # > Our main nixos configuration file <
            modules = [
              ./nixos/configuration.nix
              ./nixos/hardware-configuration.nix
              {
                inherit username;
                hostname = "omen";
              }
            ];
          };
          "pendlive" = import ./hosts/pendlive { inherit inputs pkgsFor; };
          "${username}@${hostname}-with-homemanager" = nixpkgs.lib.nixosSystem {
            specialArgs = {
              inherit inputs username hostname;
            }; # Pass flake inputs to our config
            # > Our main nixos configuration file <
            modules = [
              ./nixos/configuration.nix
              home-manager.nixosModules.home-manager
              ./home-manager/home.nix
            ];
          };
        };

        # Standalone home-manager configuration entrypoint
        # Available through 'home-manager --flake .#your-username@your-hostname'
        homeConfigurations = {
          # FIXME replace with your username@hostname
          "${username}@${hostname}" =
            home-manager.lib.homeManagerConfiguration {
              pkgs = pkgsFor system; # Home-manager requires 'pkgs' instance
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

