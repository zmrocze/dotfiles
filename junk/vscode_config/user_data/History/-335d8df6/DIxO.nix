{
  description = "Template project";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  inputs = {
    tooling.url = "github:mlabs-haskell/mlabs-tooling.nix";
    # plutus-apps = {
    #   url = "github:input-output-hk/plutus-apps/v1.1.0";
    #   flake = false;
    # };
    # plutus = {
    #   url = "github:input-output-hk/plutus/a56c96598b4b25c9e28215214d25189331087244";
    #   flake = false;
    # };
    # iohk-monitoring = {
    #   url = github:input-output-hk/iohk-monitoring-framework/0fbd28820b328085b26baad4483f48b0215ec550;
    #   flake = false;
    # };
    # plutarch.url = github:Plutonomicon/plutarch-plutus/v1.3.0;
    quickcheck-contractmodel = {
      url = github:Quviq/quickcheck-contractmodel/b66e3ff0c6011dcef63f2bcfa6ea8d6ab685230d;
      flake = false;
    };
            # all inputs below here are for pinning with haskell.nix
    # cardano-addresses = {
    #   url =
    #     "github:input-output-hk/cardano-addresses/5094fb9d304ed69adedc99513634a00cbf850fca";
    #   flake = false;
    # };
    cardano-node = {
      url =
        "github:input-output-hk/cardano-node/ebc7be471b30e5931b35f9bbc236d21c375b91bb";
      flake = false; # we need it to be available in shell
    };
    # cardano-wallet = {
    #   url = "github:input-output-hk/cardano-wallet/bbf11d4feefd5b770fb36717ec5c4c5c112aca87";
    #   flake = false;
    # };
    # hw-aeson = {
    #   url = "github:haskell-works/hw-aeson/ba7c1e41c6e54d6bf9fd1cd013489ac713fc3153";
    #   flake = false;
    # };
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
           "${inputs.cardano-node}/cardano-api"
          ];
        })
      ];
    };
}