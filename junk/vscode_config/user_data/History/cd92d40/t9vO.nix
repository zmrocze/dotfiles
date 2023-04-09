{
  description = "Mlabs Plutus Template";

  nixConfig = {
    # extra-substituters = [ "https://cache.iog.io" ];
    # extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/fcdd234cfe71345990f09eb1d6b4e2274faa2405;
    haskell-nix.follows = "plutip/haskell-nix";
    tooling.url = github:mlabs-haskell/mlabs-tooling.nix;
  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib,... }:
    let
      supportedSystems = [ "x86_64-linux" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          haskell-nix.overlay # TODO: can actualy remove?
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
        ];
        inherit (haskell-nix) config;
      };

    in {
      inherit nixpkgsFor;
      
      apps = perSystem (system:
        { 
          ctl-runtime1 = (nixpkgsFor system).launchCtlRuntime {};
          # ctl-blockfrost-runtime = launch-ctl-runtime { blockfrost.enable = true; };
          ctl-runtime2 = cardano-transaction-lib.apps.${system}.ctl-runtime;
        });
    };
}
