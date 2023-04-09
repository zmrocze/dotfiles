{
  description = "Extra module for freer-simple";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    haskell-nix-extra-hackage = {
      url = "github:mlabs-haskell/haskell-nix-extra-hackage";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-nix.follows = "haskell-nix";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      flake = false;
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , haskell-nix
    , haskell-nix-extra-hackage
    , iohk-nix
    , pre-commit-hooks
    , ...
    }:
    let
      plainNixpkgsFor = system: import nixpkgs { inherit system; };
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ haskell-nix.overlay ];
      };

      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      fourmoluFor = system: (plainNixpkgsFor system).haskellPackages.fourmolu;
      hlintFor = system: (plainNixpkgsFor system).haskellPackages.hlint_3_4_1;


      preCommitCheckFor = system:
        pre-commit-hooks.lib.${system}.run
          {
            src = ./.;

            settings = {
              ormolu.defaultExtensions = [
                "ImportQualifiedPost"
              ];
            };

            hooks = {
              cabal-fmt.enable = true;
              fourmolu.enable = true;
              nixpkgs-fmt.enable = true;
              shellcheck.enable = true;
              statix.enable = true;
              hlint.enable = false;
              markdownlint.enable = false;
            };

            tools = {
              fourmolu = fourmoluFor system;
              hlint = hlintFor system;
            };
          };

      ghcVersion = "8107";
      compiler-nix-name = "ghc" + ghcVersion;

      cabalProjectLocal = ''
        constraints: aeson >= 2, hedgehog >= 1.1
      '';

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plainPkgs = plainNixpkgsFor system;

          hls = pkgs.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; };

        in
        pkgs.haskell-nix.cabalProject' {
          src = ./.;
          index-state = "2022-02-22T20:47:03Z";
          inherit compiler-nix-name cabalProjectLocal;

          shell = {
            inherit (preCommitCheckFor system) shellHook;
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              plainPkgs.cabal-install
              plainPkgs.fd
              plainPkgs.haskellPackages.apply-refact
              plainPkgs.haskellPackages.cabal-fmt
              plainPkgs.nixpkgs-fmt

              (fourmoluFor system)
              (hlintFor system)
              hls
            ];
          };
        };
    in
    {
      inherit plainNixpkgsFor cabalProjectLocal;

      project = perSystem projectFor;
      flake = perSystem (system: self.project.${system}.flake { });

      packages = perSystem (system:
        self.flake.${system}.packages
      );

      devShells = perSystem (system: {
        default = self.flake.${system}.devShell;
        tooling =
          let
            pkgs = plainNixpkgsFor system;
          in
          pkgs.mkShell {
            inherit (preCommitCheckFor system) shellHook;
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.fd
              pkgs.haskellPackages.apply-refact
              pkgs.haskellPackages.cabal-fmt
              pkgs.nixpkgs-fmt
              (hlintFor system)
              (fourmoluFor system)
            ];
          };
      });

      checks = perSystem (system:
        self.flake.${system}.checks
        // { formatCheck = preCommitCheckFor system; }
      );

      hydraJobs = {
        inherit (self) checks packages devShells;
      };
    };
}


