{
  description = "Template project";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    plutus-apps = {
      url =
        "github:input-output-hk/plutus-apps/v1.1.0";
      flake = false;
    };
    plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/fcdd234cfe71345990f09eb1d6b4e2274faa2405;
    haskell-nix.follows = "plutip/haskell-nix";
  };

  outputs = inputs@{ self, tooling, nixpkgs, haskell-nix, plutip, cardano-transaction-lib, ... }: let
    
    supportedSystems = [ "x86_64-linux" ];
    perSystem = nixpkgs.lib.genAttrs supportedSystems;

    nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          haskell-nix.overlay
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
        ];
        inherit (haskell-nix) config;
      };
    nixpkgsFor' = system: import nixpkgs { inherit system; };
    in {
      
      inherit nixpkgsFor;

      onchain = tooling.lib.mkFlake { inherit self; }
        {
          imports = [
            (tooling.lib.mkHaskellFlakeModule1 {
              project.src = ./.;
              project.extraHackage = [
              # "${inputs.plutus-apps}/plutus-ledger" 
              # "${inputs.plutus-apps}/freer-extras"
              ];
            })
          ];
        };

      packages = perSystem (system:
        (self.onchain system).packages
      );
      checks = perSystem (system:
        self.onchain.${system}.checks
      );

      devShells = perSystem (system: {
        # why its either "flake" or "project" for the two?
        onchain = self.onchain.${system}.devShell;
      });
  };
}