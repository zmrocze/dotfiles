{
  description = "Template project";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    plutarch.url = github:Plutonomicon/plutarch-plutus;
    plutus-apps = {
      url = "github:input-output-hk/plutus-apps/v1.0.0";
      flake = false;
    };
    cardano-wallet = {
      url = github:input-output-hk/cardano-wallet/18a931648550246695c790578d4a55ee2f10463e;
      flake = false;
      };
    servant-purescript = {
      url = github:input-output-hk/servant-purescript/44e7cacf109f84984cd99cd3faf185d161826963;
      flake = false;
    };
    purescript-bridge = {
      url = github:input-output-hk/purescript-bridge/47a1f11825a0f9445e0f98792f79172efef66c00;
      flake = false;
    };
    quickcheck-dynamic = {
      url = github:input-output-hk/quickcheck-dynamic/c272906361471d684440f76c297e29ab760f6a1e;
      flake = false;
    };
    cardano-addresses = {
      url = github:input-output-hk/cardano-addresses/b7273a5d3c21f1a003595ebf1e1f79c28cd72513;
      flake = false;
    };
    cardano-ledger = {
      url = github:input-output-hk/cardano-ledger/da3e9ae10cf9ef0b805a046c84745f06643583c2;
      flake = false;
    };
    cardano-node = {
      url = github:input-output-hk/cardano-node/1.35.5;
      flake = false;
    };
  };

  outputs = inputs@{ self, tooling, ... }: tooling.lib.mkFlake { inherit self; }
    {
      imports = [
        (tooling.lib.mkHaskellFlakeModule1 {
          project.src = ./.;
          project.compiler-nix-name = "ghc8107"; 
          project.extraHackage = [
            "${inputs.cardano-node}/cardano-api"
          #  "${inputs.plutus}/plutus-ledger-api"
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