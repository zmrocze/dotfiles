{
  description = "Mlabs Plutus Template";

  nixConfig = {
    # extra-substituters = [ "https://cache.iog.io" ];
    # extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    # plutip.url = "github:mlabs-haskell/plutip/89cf822c213f6a4278a88c8a8bb982696c649e76";
    # plutip.url = github:mlabs-haskell/plutip/8364c43ac6bc9ea140412af9a23c691adf67a18b;
    cardano-transaction-lib.url = github:Plutonomicon/cardano-transaction-lib/b565f4b1ec877c671ec4ffc13b1b89dbe498bceb;
    # haskell-nix.follows = "cardano-transaction-lib/haskell-nix";
    tooling.url = github:mlabs-haskell/mlabs-tooling.nix;
    # To use the same version of `nixpkgs` as ctl does
    nixpkgs.follows = "cardano-transaction-lib/nixpkgs";

    # onchain plutarch
    # TODO: nixpkg follows?
    ply.url = github:mlabs-haskell/ply?ref=0.4.0;
    plutarch.url = "github:Plutonomicon/plutarch-plutus?ref=95e40b42a1190191d0a07e3e4e938b72e6f75268";
    psm.url = github:mlabs-haskell/plutus-simple-model;

  };

  outputs = inputs@{ self, nixpkgs, cardano-transaction-lib, tooling, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          cardano-transaction-lib.overlays.purescript
          cardano-transaction-lib.overlays.runtime
          cardano-transaction-lib.overlays.spago
        ];
      };

      offchain = {
        projectFor = system:
          let
            pkgs = nixpkgsFor system;
          in
          pkgs.purescriptProject {
            inherit pkgs;
            projectName = "mlabs-plutus-template-project";
            strictComp = false; # TODO: this should be eventually removed
            src = ./offchain;
            shell = {
              withRuntime = true;
              packageLockOnly = true;
              packages = with pkgs; [
                fd
                nodePackages.eslint
                nodePackages.prettier
              ];
              shellHook =
                ''
                  export LC_CTYPE=C.UTF-8
                  export LC_ALL=C.UTF-8
                  export LANG=C.UTF-8
                '';
            };
          };
      };
    in
    {
      inherit nixpkgsFor;

      offchain = perSystem offchain.projectFor;

      devShells = (perSystem (system: {
        offchain = self.offchain.${system}.devShell;
      }));
    };
}
