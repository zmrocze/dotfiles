{
  description = "Template project";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    cardano-node = {
      url = github:input-output-hk/cardano-node/1.35.5;
      flake = false;
    };
    plutus-apps = {
      url = github:input-output-hk/plutus-apps/671fb02a872b1060fee67f3070ad6ab893a6122e;
      flake = false;
    };
    quickcheck-contractmodel = {
      url = github:Quviq/quickcheck-contractmodel/cc43f13f98c704e0d53dbdef6a98367918f8c5c1;
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
            # "${inputs.plutus-apps}/plutus-ledger"
            # "${inputs.plutus-apps}/plutus-script-utils"
            # "${inputs.quickcheck-contractmodel}/contractmodel"
          ];
        })
      ];
    };
}
