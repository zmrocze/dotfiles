{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    # nixpkgs.url = github:NixOS/nixpkgs;

  };

  outputs = { self, nixpkgs }: let
      systems = ["x86_64-linux"];
      perSystem = nixpkgs.lib.genAttrs systems;
      in rec {

        docker-example = (system: 
            let 
              pkgsLinux = import nixpkgs { inherit system ; };
            in nixpkgs.dockerTools.buildImage {
                name = "with-zsh";
                config = {
                  Cmd = [ "${pkgsLinux.zsh}/bin/hello" ];
                };
              }
          );

        packages = perSystem (system: {
            docker-zsh = docker-example {inherit nixpkgs; };
          });
    };
}
