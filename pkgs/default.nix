{ nixpkgs, nixpkgs-23-05, my-lib }: rec {
  pkgsFor = system:
    import nixpkgs {
      inherit system;
      overlays = [
        (final: _: my-lib.lib final)
        # (final: _: local-lib'.overlay' final )
        (_: _:
          let pkgs2305 = pkgs2305For system;
          in {
            inherit (pkgs2305) aliza;
            nixpkgs-23-05.haskellPackages.cabal-plan =
              pkgs2305.haskellPackages.cabal-plan;
          })
      ];
      config = {
        permittedInsecurePackages = [ "electron-25.9.0" ];
        allowUnfree = true;
        allowUnfreePredicate = _: true;
        packageOverrides = pkgs: {
          sedutil = pkgs.sedutil.overrideAttrs (oldAttrs: {
            patches = (oldAttrs.patches or [ ]) ++ [
              # Add support for enabling unlocking when resuming from sleep
              # See: https://github.com/Drive-Trust-Alliance/sedutil/pull/190
              (builtins.fetchurl {
                url =
                  "https://patch-diff.githubusercontent.com/raw/Drive-Trust-Alliance/sedutil/pull/190.patch";
                sha256 = "1929i4998d6rd8ss0r4gi82fl2hjgh5x5b5w9y76d6842vp4n089";
              })
            ];
          });
        };
      };
    };
  pkgs2305For = system:
    import nixpkgs-23-05 {
      inherit system;
      config = {
        allowUnfree = true;
        allowUnfreePredicate = _: true;
      };
    };
}
