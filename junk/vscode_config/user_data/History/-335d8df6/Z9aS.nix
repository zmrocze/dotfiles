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
      url = "github:input-output-hk/plutus-apps/v1.1.0";
      flake = false;
    };
    plutus = {
      url = "github:input-output-hk/plutus/a56c96598b4b25c9e28215214d25189331087244";
      flake = false;
    };
    iohk-monitoring = {
      url = github:input-output-hk/iohk-monitoring/0fbd28820b328085b26baad4483f48b0215ec550;
      flake = false;
    };
    plutarch.url = github:Plutonomicon/plutarch-plutus/v1.3.0;
    quickcheck-contractmodel = {
      url = github:Quviq/quickcheck-contractmodel/b66e3ff0c6011dcef63f2bcfa6ea8d6ab685230d;
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
           "${inputs.plutus-apps}/plutus-ledger" 
           "${inputs.plutus-apps}/plutus-script-utils" 
           "${inputs.plutus-apps}/freer-extras"
           "${inputs.plutarch}"
           "${inputs.quickcheck-contractmodel}/contractmodel"
           "${iohk-monitoring}/iohk-monitoring"
          ];
        })
      ];
    };
}