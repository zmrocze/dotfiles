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
      url = "github:input-output-hk/plutus-apps/v1.0.0";
      flake = false;
    };
    cardano-wallet = {
      url = github:input-output-hk/cardano-wallet;
      flake = false;
      };
    servant-purescript = {
      url = github:input-output-hk/servant-purescript;
      flake = false;
    };
    purescript-bridge = {
      url = github:input-output-hk/purescript-bridge;
      flake = false;
    };
    quickcheck-dynamic = {
      url = github:input-output-hk/quickcheck-dynamic;
      flake = false;
    };
    cardano-addresses = {
      url = github:input-output-hk/cardano-addresses;
      flake = false;
    };
    cardano-ledger = {
      url = github:input-output-hk/cardano-ledger;
      flake = false;
    };
  };

  outputs = inputs@{ self, tooling, ... }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;
          project.extraHackage = [
           # "${inputs.plutus}/plutus-ledger-api"
          #  "${inputs.plutus-apps}/plutus-ledger" 
          #  "${inputs.plutus-apps}/plutus-script-utils" 
          #  "${inputs.plutus-apps}/freer-extras"
          #  "${inputs.plutarch}"
          #  "${inputs.quickcheck-contractmodel}/contractmodel"
          #  "${inputs.iohk-monitoring}/iohk-monitoring"
          #  "${inputs.cardano-node}/cardano-api"
          ];
        })
      ];
    };
}