{
  description = "bot-plutus-interface";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, haskell-nix, iohk-nix, CHaP, ... }@inputs:
    let
      defaultSystems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system:
        import nixpkgs {
          overlays = [
            haskell-nix.overlay
            iohk-nix.overlays.crypto
          ];
          inherit (haskell-nix) config;
          inherit system;
        };
      nixpkgsFor' = system: import nixpkgs { inherit system; };

      haskellModules = [
        ({ pkgs, ... }:
          {
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-script-utils.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
              cardano-wallet-core.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
              cardano-config.components.library.build-tools = [
                pkgs.buildPackages.buildPackages.gitMinimal
              ];
            };
          }
        )
      ];

      extraSources = [
        {
          src = inputs.cardano-addresses;
          subdirs = [ "core" "command-line" ];
        }
        {
          src = inputs.cardano-base;
          subdirs = [
            "base-deriving-via"
            "binary"
            "binary/test"
            "cardano-crypto-class"
            "cardano-crypto-praos"
            "cardano-crypto-tests"
            "measures"
            "orphans-deriving-via"
            "slotting"
            "strict-containers"
          ];
        }
        {
          src = inputs.cardano-crypto;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-ledger;
          subdirs = [
            "eras/alonzo/impl"
            "eras/alonzo/test-suite"
            "eras/babbage/impl"
            "eras/byron/chain/executable-spec"
            "eras/byron/crypto"
            "eras/byron/crypto/test"
            "eras/byron/ledger/executable-spec"
            "eras/byron/ledger/impl"
            "eras/byron/ledger/impl/test"
            "eras/shelley/impl"
            "eras/shelley/test-suite"
            "eras/shelley-ma/impl"
            "eras/shelley-ma/test-suite"
            "libs/cardano-data"
            "libs/cardano-ledger-core"
            "libs/cardano-ledger-pretty"
            "libs/cardano-protocol-tpraos"
            "libs/vector-map"
            "libs/non-integral"
            "libs/set-algebra"
            "libs/small-steps"
            "libs/small-steps-test"
          ];
        }
        {
          src = inputs.cardano-node;
          subdirs = [
            "cardano-api"
            "cardano-cli"
            "cardano-git-rev"
            "cardano-node"
            "cardano-submit-api"
            "cardano-testnet"
            "trace-dispatcher"
            "trace-forward"
            "trace-resources"
          ];
        }
        {
          src = inputs.cardano-config;
          subdirs = [ "." ];
        }
        {
          src = inputs.cardano-prelude;
          subdirs = [ "cardano-prelude" "cardano-prelude-test" ];
        }
        {
          src = inputs.cardano-wallet;
          subdirs = [
            "lib/cli"
            "lib/core"
            "lib/core-integration"
            "lib/dbvar"
            "lib/launcher"
            "lib/numeric"
            "lib/shelley"
            "lib/strict-non-empty-containers"
            "lib/test-utils"
            "lib/text-class"
          ];
        }
        {
          src = inputs.ekg-forward;
          subdirs = [ "." ];
        }
        {
          src = inputs.ekg-json;
          subdirs = [ "." ];
        }
        {
          src = inputs.flat;
          subdirs = [ "." ];
        }
        {
          src = inputs.goblins;
          subdirs = [ "." ];
        }
        {
          src = inputs.hedgehog-extras;
          subdirs = [ "." ];
        }
        {
          src = inputs.hw-aeson;
          subdirs = [ "." ];
        }
        {
          src = inputs.iohk-monitoring-framework;
          subdirs = [
            "contra-tracer"
            "iohk-monitoring"
            "tracer-transformers"
            "plugins/backend-ekg"
            "plugins/backend-aggregation"
            "plugins/backend-monitoring"
            "plugins/backend-trace-forwarder"
          ];
        }
        {
          src = inputs.io-sim;
          subdirs = [
            "io-classes"
            "io-sim"
            "strict-stm"
          ];
        }
        {
          src = inputs.optparse-applicative;
          subdirs = [ "." ];
        }
        {
          src = inputs.ouroboros-network;
          subdirs = [
            "monoidal-synchronisation"
            "network-mux"
            "ntp-client"
            "ouroboros-consensus"
            "ouroboros-consensus-byron"
            "ouroboros-consensus-cardano"
            "ouroboros-consensus-protocol"
            "ouroboros-consensus-shelley"
            "ouroboros-network"
            "ouroboros-network-framework"
            "ouroboros-network-testing"
          ];
        }
        {
          src = inputs.plutus-core;
          subdirs = [
            "plutus-core"
            "plutus-ledger-api"
            "plutus-tx"
            "plutus-tx-plugin"
            "prettyprinter-configurable"
            "stubs/plutus-ghc-stub"
            "word-array"
          ];
        }
        {
          src = inputs.plutus-apps;
          subdirs = [
            "cardano-streaming"
            "doc"
            "freer-extras"
            "marconi"
            "marconi-mamba"
            "playground-common"
            "pab-blockfrost"
            "plutus-chain-index"
            "plutus-chain-index-core"
            "plutus-contract"
            "plutus-contract-certification"
            "plutus-example"
            "plutus-ledger"
            "plutus-ledger-constraints"
            "plutus-pab"
            "plutus-pab-executables"
            "plutus-playground-server"
            "plutus-script-utils"
            "plutus-tx-constraints"
            "plutus-use-cases"
            "rewindable-index"
            "web-ghc"
          ];
        }
        {
          src = inputs.purescript-bridge;
          subdirs = [ "." ];
        }
        {
          src = inputs.quickcheck-dynamic;
          subdirs = [ "." ];
        }
        {
          src = inputs.servant-purescript;
          subdirs = [ "." ];
        }
        {
          src = inputs.typed-protocols;
          subdirs = [
            "typed-protocols"
            "typed-protocols-cborg"
            "typed-protocols-examples"
          ];
        }
        {
          src = inputs.Win32-network;
          subdirs = [ "." ];
        }
      ];

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          pkgs' = nixpkgsFor' system;
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
            name = "bot-plutus-interface";
            compiler-nix-name = "ghc8107";
            shell = {
              additional = ps: [
                ps.plutus-pab
              ];
              withHoogle = true;
              tools.haskell-language-server = { };
              exactDeps = true;
              nativeBuildInputs = with pkgs'; [
                cabal-install
                haskellPackages.cabal-fmt
                haskellPackages.implicit-hie
                haskellPackages.fourmolu
                haskellPackages.apply-refact
                hlint
                jq
                websocat
                fd
                nixpkgs-fmt
                project.hsPkgs.cardano-cli.components.exes.cardano-cli
              ];
            };
            modules = haskellModules;
          };
        in
        project;

      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          { nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ]; } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint_check
          mkdir $out
        '';

    in
    {
      inherit haskellModules;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      defaultPackage = perSystem (system:
        let lib = "bot-plutus-interface:lib:bot-plutus-interface";
        in self.flake.${system}.packages.${lib});

      packages = perSystem (system: self.flake.${system}.packages);

      apps = perSystem (system: self.flake.${system}.apps);

      devShell = perSystem (system: self.flake.${system}.devShell);

      # This will build all of the project's executables and the tests
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-check"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system}
              ++ builtins.attrValues self.flake.${system}.packages
              ++ [ self.devShell.${system}.inputDerivation ];
          } "touch $out");
      # NOTE `nix flake check` will not work at the moment due to use of
      # IFD in haskell.nix
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });

      # Instruction for the Hercules CI to build on x86_64-linux only, to avoid errors about systems without agents.
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
