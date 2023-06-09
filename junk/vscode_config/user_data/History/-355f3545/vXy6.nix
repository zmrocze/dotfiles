{
  description = "cardano-transaction-lib";

  inputs = {
    iohk-nix.follows = "ogmios/iohk-nix";
    haskell-nix.follows = "ogmios/haskell-nix";
    nixpkgs.follows = "ogmios/nixpkgs";
    CHaP.follows = "ogmios/CHaP";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    ogmios.url = "github:mlabs-haskell/ogmios/3b229c1795efa30243485730b78ea053992fdc7a";

    plutip.url = "github:zmrocze/plutip/1d3a9605b0a46899e83395e49c733d66ce516ee4";
    plutip.inputs.bot-plutus-interface.follows = "bot-plutus-interface";
    plutip.inputs.haskell-nix.follows = "bot-plutus-interface/haskell-nix";
    plutip.inputs.iohk-nix.follows = "bot-plutus-interface/iohk-nix";
    plutip.inputs.nixpkgs.follows = "bot-plutus-interface/haskell-nix/nixpkgs";

    bot-plutus-interface = {
      url = "github:mlabs-haskell/bot-plutus-interface?rev=7235aa6fba12b0cf368d9976e1e1b21ba642c038";
      inputs.cardano-wallet.follows = "cardano-wallet";
    };

    kupo-nixos.url = "github:mlabs-haskell/kupo-nixos/438799a67d0e6e17f21b7b3d0ae1b6325e505c61";
    kupo-nixos.inputs.kupo.follows = "kupo";

    kupo = {
      url = "github:CardanoSolutions/kupo/v2.2.0";
      flake = false;
    };

    cardano-wallet.url = "github:mlabs-haskell/cardano-wallet?rev=9d34b2633ace6aa32c1556d33c8c2df63dbc8f5b";

    ogmios-datum-cache.url = "github:mlabs-haskell/ogmios-datum-cache/862c6bfcb6110b8fe816e26b3bba105dfb492b24";

    # ogmios and ogmios-datum-cache nixos modules (remove and replace with the above after merging and updating)
    ogmios-nixos.url = "github:mlabs-haskell/ogmios";
    ogmios-datum-cache-nixos.url = "github:mlabs-haskell/ogmios-datum-cache/marton/nixos-module";

    cardano-node.follows = "ogmios-nixos/cardano-node";
    # for new environments like preview and preprod. TODO: remove this when cardano-node is updated
    iohk-nix-environments.url = "github:input-output-hk/iohk-nix";
    cardano-node.inputs.iohkNix.follows = "iohk-nix-environments";

    # Repository with network parameters
    cardano-configurations = {
      # Override with "path:/path/to/cardano-configurations";
      url = "github:input-output-hk/cardano-configurations";
      flake = false;
    };
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix/da7acb2662961fd355f0a01a25bd32bf33577fa8";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , iohk-nix
    , cardano-configurations
    , CHaP
    , ...
    }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      mkNixpkgsFor = system: import nixpkgs {
        overlays = nixpkgs.lib.attrValues self.overlays ++ [
          (_: _: {
            ogmios-fixtures = inputs.ogmios;
          })
        ];
        inherit (haskell-nix) config;
        inherit system;
      };

      inherit (import ./nix/runtime.nix { inherit inputs; })
        buildCtlRuntime launchCtlRuntime;

      allNixpkgs = perSystem mkNixpkgsFor;

      nixpkgsFor = system: allNixpkgs.${system};

      buildOgmiosFixtures = pkgs: pkgs.runCommand "ogmios-fixtures"
        {
          buildInputs = [ pkgs.jq pkgs.pcre ];
        }
        ''
          cp -r ${pkgs.ogmios-fixtures}/server/test/vectors vectors
          chmod -R +rwx .

          function on_file () {
            local path=$1
            local parent="$(basename "$(dirname "$path")")"
            if command=$(pcregrep -o1 -o2 -o3 'Query\[(.*)\]|(EvaluateTx)|(SubmitTx)' <<< "$path")
            then
              echo "$path"
              json=$(jq -c .result "$path")
              md5=($(md5sum <<< "$json"))
              printf "%s" "$json" > "ogmios/$command-$md5.json"
            fi
          }
          export -f on_file

          mkdir ogmios
          find vectors/ -type f -name "*.json" -exec bash -c 'on_file "{}"' \;
          mkdir $out
          cp -rT ogmios $out
        '';

      psProjectFor = pkgs:
        let
          projectName = "cardano-transaction-lib";
          # `filterSource` will still trigger rebuilds with flakes, even if a
          # filtered path is modified as the output path name is impurely
          # derived. Setting an explicit `name` with `path` helps mitigate this
          src = builtins.path {
            path = self;
            name = "${projectName}-src";
            filter = path: ftype:
              !(pkgs.lib.hasSuffix ".md" path)
              && !(ftype == "directory" && builtins.elem
                (baseNameOf path) [ "server" "doc" ]
              );
          };
          ogmiosFixtures = buildOgmiosFixtures pkgs;
          project = pkgs.purescriptProject {
            inherit src pkgs projectName;
            packageJson = ./package.json;
            packageLock = ./package-lock.json;
            shell = {
              withRuntime = true;
              shellHook = exportOgmiosFixtures;
              packageLockOnly = true;
              packages = with pkgs; [
                arion
                fd
                haskellPackages.fourmolu
                nixpkgs-fmt
                nodePackages.eslint
                nodePackages.prettier
                file
              ];
            };
          };
          exportOgmiosFixtures =
            ''
              export OGMIOS_FIXTURES="${ogmiosFixtures}"
            '';
        in
        rec {
          packages = {
            ctl-example-bundle-web = project.bundlePursProject {
              main = "Ctl.Examples.ByUrl";
              entrypoint = "examples/index.js";
            };

            ctl-runtime = pkgs.arion.build {
              inherit pkgs;
              modules = [ (buildCtlRuntime pkgs { }) ];
            };

            docs = project.buildSearchablePursDocs {
              packageName = projectName;
            };
          };

          checks = {
            ctl-plutip-test = project.runPlutipTest {
              name = "ctl-plutip-test";
              testMain = "Test.Ctl.Plutip";
              # After updating `PlutipConfig` this can be set for now:
              # withCtlServer = false;
              env = { OGMIOS_FIXTURES = "${ogmiosFixtures}"; };
            };
            ctl-staking-test = project.runPlutipTest {
              name = "ctl-staking-test";
              testMain = "Test.Ctl.Plutip.Staking";
            };
            ctl-unit-test = project.runPursTest {
              name = "ctl-unit-test";
              testMain = "Test.Ctl.Unit";
              env = { OGMIOS_FIXTURES = "${ogmiosFixtures}"; };
            };
          };

          devShell = project.devShell;

          apps = {
            docs = project.launchSearchablePursDocs {
              builtDocs = packages.docs;
            };
          };
        };

      hsProjectFor = pkgs: import ./server/nix {
        inherit inputs pkgs CHaP;
        inherit (pkgs) system;
        src = ./server;
      };
    in
    {
      overlay = builtins.trace
        (
          "warning: `cardano-transaction-lib.overlay` is deprecated and will be"
          + " removed in the next release. Please use"
          + " `cardano-transaction-lib.overlays.{runtime, purescript, ctl-server}`"
          + " directly instead"
        )
        nixpkgs.lib.composeManyExtensions
        (nixpkgs.lib.attrValues self.overlays);

      overlays = with inputs; {
        purescript = final: prev: {
          easy-ps = import inputs.easy-purescript-nix { pkgs = final; };
          purescriptProject = import ./nix { pkgs = final; };
        };
        spago = final: prev: {
          easy-ps = prev.easy-ps // {
            spago = prev.easy-ps.spago.overrideAttrs (_: rec {
              version = "0.20.7";
              src =
                if final.stdenv.isDarwin
                then
                  final.fetchurl
                    {
                      url = "https://github.com/purescript/spago/releases/download/${version}/macOS.tar.gz";
                      sha256 = "0s5zgz4kqglsavyh7h70zmn16vayg30alp42w3nx0zwaqkp79xla";
                    }
                else
                  final.fetchurl {
                    url = "https://github.com/purescript/spago/releases/download/${version}/Linux.tar.gz";
                    sha256 = "0bh15dr1fg306kifqipnakv3rxab7hjfpcfzabw7vmg0gsfx8xka";
                  };
            });
          };
        };
        # This is separate from the `runtime` overlay below because it is
        # optional (it's only required if using CTL's `applyArgs` effect).
        # Including it by default in the `overlays.runtime` also requires that
        # `prev` include `haskell-nix.overlay` and `iohk-nix.overlays.crypto`;
        # this is not ideal to force upon all users
        ctl-server = nixpkgs.lib.composeManyExtensions [
          (
            final: prev:
              # if `haskell-nix.overlay` has not been applied, we cannot use the
              # package set to build the `hsProjectFor`. We don't want to always
              # add haskell.nix's overlay or use the `ctl-server` from our own
              # `outputs.packages` because this might lead to conflicts with the
              # `hackage.nix` version being used (this might also happen with the
              # Ogmios and Plutip packages, but at least we have direct control over
              # our own haskell.nix project)
              #
              # We can check for the necessary attribute and then apply the
              # overlay if necessary
              nixpkgs.lib.optionalAttrs (!(prev ? haskell-nix))
                (haskell-nix.overlay final prev)

          )
          (
            final: prev:
              # Similarly, we need to make sure that `libsodium-vrf` is available
              # for the Haskell server
              nixpkgs.lib.optionalAttrs (!(prev ? libsodium-vrf))
                (iohk-nix.overlays.crypto final prev)
          )
          (
            final: prev: {
              ctl-server =
                (hsProjectFor final).hsPkgs.ctl-server.components.exes.ctl-server;
            }
          )
        ];
        runtime = nixpkgs.lib.composeManyExtensions [
          (
            final: prev:
              let
                inherit (prev) system;
              in
              {
                plutip-server =
                  inputs.plutip.packages.${system}."plutip:exe:plutip-server";
                ogmios-datum-cache =
                  inputs.ogmios-datum-cache.defaultPackage.${system};
                ogmios = ogmios.packages.${system}."ogmios:exe:ogmios";
                kupo = inputs.kupo-nixos.defaultPackage.${system};
                buildCtlRuntime = buildCtlRuntime final;
                launchCtlRuntime = launchCtlRuntime final;
                inherit cardano-configurations;
              }
          )
          (
            final: prev: nixpkgs.lib.optionalAttrs (!(prev ? ctl-server))
              (
                builtins.trace
                  (
                    "Warning: `ctl-server` has moved to `overlays.ctl-server`"
                    + " and will be removed from `overlays.runtime` soon"
                  )
                  (self.overlays.ctl-server final prev)
              )
          )

        ];
      };

      # flake from haskell.nix project
      hsFlake = perSystem (system: (hsProjectFor (nixpkgsFor system)).flake { });

      devShells = perSystem (system: {
        # This is the default `devShell` and can be run without specifying
        # it (i.e. `nix develop`)
        default = (psProjectFor (nixpkgsFor system)).devShell;
        # It might be a good idea to keep this as a separate shell; if you're
        # working on the PS frontend, it doesn't make a lot of sense to pull
        # in all of the Haskell dependencies
        #
        # This can be used with `nix develop .#hsDevShell
        hsDevShell = self.hsFlake.${system}.devShell;
      });

      packages = perSystem (system:
        self.hsFlake.${system}.packages
        // (psProjectFor (nixpkgsFor system)).packages
      );

      apps = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor pkgs).apps // {
          inherit (self.hsFlake.${system}.apps) "ctl-server:exe:ctl-server";
          ctl-runtime = pkgs.launchCtlRuntime { };
          default = self.apps.${system}.ctl-runtime;
          vm = {
            type = "app";
            program =
              "${self.nixosConfigurations.test.config.system.build.vm}/bin/run-nixos-vm";
          };
        });

      # TODO
      # Add a check that attempts to verify if the scaffolding template is
      # reasonably up-to-date. See:
      # https://github.com/Plutonomicon/cardano-transaction-lib/issues/839
      checks = perSystem (system:
        let
          pkgs = nixpkgsFor system;
        in
        (psProjectFor pkgs).checks
        // self.hsFlake.${system}.checks
        // {
          formatting-check = pkgs.runCommand "formatting-check"
            {
              nativeBuildInputs = with pkgs; [
                easy-ps.purs-tidy
                haskellPackages.fourmolu
                nixpkgs-fmt
                nodePackages.prettier
                nodePackages.eslint
                fd
                file
              ];
            }
            ''
              cd ${self}
              make check-format
              touch $out
            '';
          template-deps-json = pkgs.runCommand "template-deps-check"
            {
              ctlPackageJson = builtins.readFile ./package.json;
              ctlScaffoldPackageJson = builtins.readFile ./templates/ctl-scaffold/package.json;
              nativeBuildInputs = [ pkgs.jq ];
            } ''
            cd ${self}
            diff <(jq -S .dependencies <<< $ctlPackageJson) <(jq -S .dependencies <<< $ctlScaffoldPackageJson)
            diff <(jq -S .devDependencies <<< $ctlPackageJson) <(jq -S .devDependencies <<< $ctlScaffoldPackageJson)
            touch $out
          '';
          template-dhall-diff = pkgs.runCommand "template-dhall-diff-check"
            (with builtins;
            let
              ctlPkgsExp = import ./spago-packages.nix { inherit pkgs; };
              ctlScaffoldPkgsExp = import ./templates/ctl-scaffold/spago-packages.nix { inherit pkgs; };
              ctlPs = attrValues ctlPkgsExp.inputs;
              ctlScaffoldPs = filter (p: p.name != "cardano-transaction-lib")
                (attrValues ctlScaffoldPkgsExp.inputs);
              intersection = pkgs.lib.lists.intersectLists ctlPs ctlScaffoldPs;
              scaffoldDisjoint = pkgs.lib.lists.subtractLists intersection ctlScaffoldPs;
              ctlDisjoint = pkgs.lib.lists.subtractLists intersection ctlPs;
            in
            {
              inherit ctlDisjoint scaffoldDisjoint;
              nativeBuildInputs = [ ];
            }
            ) ''

            if [ -z "$ctlDisjoint" ] && [ -z "$scaffoldDisjoint" ];
            then
              touch $out
            else
              if [ -n "$ctlDisjoint" ];
              then
                echo "The following packages are in the main projects dependencies but not in the scaffold:"
                for p in $ctlDisjoint; do
                  echo "  $p"
                done
              fi
              if [ -n "$scaffoldDisjoint" ];
              then
                echo "The following packages are in the scaffold projects dependencies but not in the main:"
                for p in $scaffoldDisjoint; do
                  echo "  $p"
                done
              fi
              exit 1
            fi
          '';
          template-version = pkgs.runCommand "template-consistent-version-check"
            (
              let
                ctlScaffoldPackages = import ./templates/ctl-scaffold/spago-packages.nix { inherit pkgs; };
                ctlScaffoldFlake = import ./templates/ctl-scaffold/flake.nix;
                versionCheck = ctlScaffoldPackages.inputs."cardano-transaction-lib".version == ctlScaffoldFlake.inputs.ctl.rev;
              in
              {
                packagesLibRev = ctlScaffoldPackages.inputs."cardano-transaction-lib".version;
                flakeLibRev = ctlScaffoldFlake.inputs.ctl.rev;
                nativeBuildInputs = [ ];
              }
            ) ''

            if [ $packagesLibRev != $flakeLibRev ]
            then
              echo "CTL revision in scaffold flake.nix ($flakeLibRev) doesn't match revision referenced in spago-packages.nix ($packagesLibRev). Please update flake.nix or packages.dhall and run spago2nix."
              exit 1
            fi
            touch $out
          '';
          examples-imports-check = pkgs.runCommand "examples-imports-check" { }
            ''
              cd ${self}
              make check-examples-imports
              touch $out
            '';
        });

      templatePath = builtins.toString self + self.templates.ctl-scaffold.path;

      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            combined =
              builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.packages.${system};
          }
          ''
            echo $combined
            touch $out
          ''
      );

      templates = {
        default = self.templates.ctl-scaffold;
        ctl-scaffold = {
          path = ./templates/ctl-scaffold;
          description = "A minimal CTL-based scaffold project";
          welcomeText = ''
            Welcome to your new CTL project!

            To enter the Nix environment and start working on it, run `nix develop`

            Please also see our

            - [Documentation](https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/doc)

            - [Generated docs](https://plutonomicon.github.io/cardano-transaction-lib/)

            - [Discord server](https://discord.gg/JhbexnV9Pc)

            If you encounter problems and/or want to report a bug, you can open
            an issue [here](https://github.com/Plutonomicon/cardano-transaction-lib/issues).

            Please search for existing issues beforehand!

          '';
        };
      };

      nixosModules.ctl-server = { pkgs, lib, ... }: {
        imports = [ ./nix/ctl-server-nixos-module.nix ];
        nixpkgs.overlays = [
          (_: _: {
            ctl-server = self.packages.${pkgs.system}."ctl-server:exe:ctl-server";
          })
        ];
      };

      nixosConfigurations.test = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.cardano-node.nixosModules.cardano-node
          inputs.ogmios-nixos.nixosModules.ogmios
          {
            services.ogmios.package =
              inputs.ogmios.packages.x86_64-linux."ogmios:exe:ogmios";
          }
          inputs.ogmios-datum-cache-nixos.nixosModules.ogmios-datum-cache
          {
            services.ogmios-datum-cache.package =
              inputs.ogmios-datum-cache.packages.x86_64-linux."ogmios-datum-cache";
          }
          self.nixosModules.ctl-server
          ./nix/test-nixos-configuration.nix
        ];
        specialArgs = {
          inherit (inputs) cardano-configurations;
        };
      };

      hydraJobs = perSystem
        (system:
          self.checks.${system}
            // self.packages.${system}
            // self.devShells.${system}
        ) // { vm = self.nixosConfigurations.test.config.system.build.vm; };
    };
}
